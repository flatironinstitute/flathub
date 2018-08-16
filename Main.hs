{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Arrow (first, second)
import           Control.Exception (throwIO)
import           Control.Monad ((<=<), forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default (Default(def))
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe, isNothing, isJust)
import qualified Data.Text as T
import qualified Data.Yaml as YAML
import qualified Network.HTTP.Client as HTTP
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import qualified Waimwork.Config as C
import           Waimwork.Response (response)
import           Waimwork.Warp (runWaimwork)
import qualified Web.Route.Invertible as R
import           Web.Route.Invertible.Wai (routeWaiError)

import Field
import Catalog
import Global
import qualified ES
import Static
import Query
import Ingest
import Compression

html :: Wai.Request -> H.Markup -> Wai.Response
html req h = okResponse [] $ H.docTypeHtml $ do
  H.head $ do
    forM_ ([["jspm_packages", if isdev then "system.src.js" else "system.js"], ["jspm.config.js"]] ++ if isdev then [["dev.js"]] else [["index.js"]]) $ \src ->
      H.script H.! HA.type_ "text/javascript" H.! HA.src (staticURI src) $ mempty
    -- TODO: use System.resolve:
    forM_ [["jspm_packages", "npm", "datatables.net-dt@1.10.19", "css", "jquery.dataTables.css"], ["main.css"]] $ \src ->
      H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href (staticURI src)
    H.script H.! HA.type_ "text/javascript" H.! HA.src "//cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3/MathJax.js?config=TeX-AMS_CHTML" $ mempty
  H.body $ do
    H.h1 $ H.text "ASTROSIMS"
    H.div H.! HA.id "bar" $ do
        H.ul H.! HA.id "topbar" $ do
            H.li $ H.text " "
            H.li $ do
                H.a H.! HA.href (WH.routeActionValue top () mempty) $ H.text "Home"
            H.li $ do
                H.div H.! HA.class_ "dropdown" $ do
                    H.a H.! HA.href (WH.routeActionValue top () mempty) $ H.text "Catalogs"
                    H.div H.! HA.class_"dropdown-content" $ do
                        H.a H.! HA.href "gaea" $ H.text "GAEA"
                        H.a H.! HA.href "lusam" $ H.text "LU-SAM"
                        H.a H.! HA.href "scsam" $ H.text "SC-SAM"
                        H.a H.! HA.href "unimach" $ H.text "UM"
                        H.a H.! HA.href "/web/candels.html" $ H.text "CANDELS"
            H.li $ do
                H.a H.! HA.href (WH.routeActionValue top () mempty) $ H.text "About"
    h
    H.footer $ do
        H.a H.! HA.href "https://github.com/flatironinstitute/astrosims-reproto" $
            H.img H.! HA.src (staticURI ["github.png"])

  where
  isdev = any ((==) "dev" . fst) $ Wai.queryString req

acceptable :: [BS.ByteString] -> Wai.Request -> Maybe BS.ByteString
acceptable l = find (`elem` l) . foldMap parseHttpAccept . lookup hAccept . Wai.requestHeaders

-- Here is where the main page is generated
top :: Route ()
top = getPath R.unit $ \() req -> do
  cats <- asks (catalogMap . globalCatalogs)
  return $ html req $
    H.dl $
      forM_ (HM.toList cats) $ \(sim, cat) -> do
        H.dt $ H.a H.! HA.href (WH.routeActionValue simulation sim mempty) $
          H.text $ catalogTitle cat
        mapM_ (H.dd . H.preEscapedText) $ catalogDescr cat

simulation :: Route Simulation
simulation = getPath R.parameter $ \sim req -> do
  cat <- askCatalog sim
  let 
    (_, quri) = routeActionURI simulation sim
    fields = catalogFieldGroups cat
    fields' = catalogFields cat
    jcat = J.pairs $
         "title" J..= catalogTitle cat
      <> "descr" J..= catalogDescr cat
      <> "uri" J..= show quri
      <> "bulk" J..= map (J.String . R.renderParameter) [BulkCSV Nothing, BulkCSV (Just CompressionGZip), BulkNumpy Nothing, BulkNumpy (Just CompressionGZip)]
      <> "fields" J..= fields'
    fieldBody :: Word -> FieldGroup -> H.Html
    fieldBody d f = H.span WH.!? (HA.title . H.textValue <$> fieldDescr f) $ do
      -- Writes the title to the span
      H.text $ fieldTitle f
      -- Writes the unit to the span
      forM_ (fieldUnits f) $ \u -> do
        if d > 1 then H.br else " "
        H.span H.! HA.class_ "units" $ "[" <> H.preEscapedText u <> "]"
    field :: Word -> FieldGroup -> FieldGroup -> H.Html
    field d f' f@Field{ fieldSub = Nothing } = do
      H.th
          H.! HA.rowspan (H.toValue d)
          H.! H.dataAttribute "data" (H.textValue $ fieldName f')
          H.! H.dataAttribute "name" (H.textValue $ fieldName f')
          H.! H.dataAttribute "type" (dtype $ fieldType f)
          H.!? (not (fieldDisp f), H.dataAttribute "visible" "false")
          H.! H.dataAttribute "default-content" mempty $ do
        fieldBody d f
    field _ _ f@Field{ fieldSub = Just s } = do
      H.th
          H.! HA.colspan (H.toValue $ length $ expandFields s) $
        fieldBody 1 f
    row :: Word -> [(FieldGroup -> FieldGroup, FieldGroup)] -> H.Html
    row d l = do
      H.tr $ mapM_ (\(p, f) -> field d (p f) f) l
      when (d > 1) $ row (pred d) $ foldMap (\(p, f) -> foldMap (fmap (p . mappend f, ) . V.toList) $ fieldSub f) l
    query = parseQuery req

    subfield :: FieldGroup -> FieldGroup -> Int -> H.Html
    subfield f g d = do
        H.tr H.! HA.id "row_" $ do
          --  H.td H.! HA.id "checkbox" $ H.div $ do
            H.td H.! HA.id (H.stringValue("depth_" <> show d)) $ H.div $ do
                H.span $ H.input H.! HA.type_ "checkbox"
                    H.! HA.id ("hide-" <> H.textValue (fieldName f))
                    H.! HA.onclick "return hide_show(event)"
                H.text (fieldTitle g)
            H.td H.! HA.id "units" $ foldMap H.text (fieldUnits g)
            H.td $ H.string (show $ fieldType g)
            H.td $ foldMap H.text (fieldDescr g)
        forM_ (fold (fieldSub g)) $ \sf ->
            subfield (f <> sf) sf (d+1)

  case acceptable ["application/json", "text/html"] req of
    Just "application/json" ->
      return $ okResponse [] jcat
    _ -> return $ html req $ do
      H.script $ do
        "Catalog="
        H.preEscapedBuilder $ J.fromEncoding jcat
        ";Query="
        H.unsafeLazyByteString $ J.encode query
        ";"
      H.h2 $ H.text $ catalogTitle cat
      mapM_ (H.p . H.preEscapedText) $ catalogDescr cat
      H.p $ "Query and explore a subset using the filters, download your selection using the link below, or get the full dataset above."
      H.table H.! HA.id "filt" $ mempty
      H.div H.! HA.id "dhist" $ do
        forM_ ['x','y'] $ \xy -> let xyv = H.stringValue [xy] in
          H.div H.! HA.id ("dhist-" <> xyv) H.! HA.class_ "dhist-xy" $
            H.button H.! HA.id ("dhist-" <> xyv <> "-tog") H.! HA.class_ "dhist-xy-tog" $
              "lin/log"
        H.canvas H.! HA.id "hist" $ mempty

      H.table H.! HA.id "tcat" H.! HA.class_ "compact" $ do
        H.thead $ row (fieldsDepth fields) ((id ,) <$> V.toList fields)
        H.tfoot $ H.tr $ H.td H.! HA.colspan (H.toValue $ length fields') H.! HA.class_ "loading" $ "loading..."

      H.div $ do
        H.p $ "Table of fields, units, and their descriptions. Click the checkbox to view / hide the specific field:"
        H.button H.! HA.id "show_button" H.! HA.onclick "return div_display(tdict)" $ "show/hide"
      H.div $ H.table H.! HA.id "tdict" H.! HA.class_ "tdict" $ do
        H.thead $ H.tr $ do
            H.th $ H.text "Field"
            H.th $ H.text "Units"
            H.th $ H.text "Type"
            H.th $ H.text "Description"
        forM_ (catalogFieldGroups cat) $ \f -> do
            subfield f f 0

      H.div $ do
        H.p $ "Generate python code to use the above filters on your local machine:"
        H.button H.! HA.id "show_button" H.! HA.onclick "return div_display(py)" $ "show/hide"
      H.div H.! HA.id "py" $ "Hello, world!"

  where
  dtype (Long _) = "num"
  dtype (Integer _) = "num"
  dtype (Short _) = "num"
  dtype (Byte _) = "num"
  dtype (Double _) = "num"
  dtype (Float _) = "num"
  dtype (HalfFloat _) = "num"
  -- dtype (Date _) = "date"
  dtype _ = "string"

import Html


routes :: R.RouteMap Action
routes = R.routes
  [ R.routeNormCase top
  , R.routeNormCase static
  , R.routeNormCase staticHtml
  , R.routeNormCase simulation
  , R.routeNormCase catalog
  , R.routeNormCase catalogBulk
  ]

data Opts = Opts
  { optConfig :: FilePath
  , optCreate :: [Simulation]
  , optIngest :: Maybe Simulation
  , optConstFields :: [(T.Text, T.Text)]
  }

instance Default Opts where
  def = Opts "config" [] Nothing []

optDescr :: [Opt.OptDescr (Opts -> Opts)]
optDescr =
  [ Opt.Option "f" ["config"] (Opt.ReqArg (\c o -> o{ optConfig = c }) "FILE") "Configuration file [config]"
  , Opt.Option "s" ["create"] (Opt.ReqArg (\i o -> o{ optCreate = T.pack i : optCreate o }) "SIM") "Create storage schema for the simulation"
  , Opt.Option "i" ["ingest"] (Opt.ReqArg (\i o -> o{ optIngest = Just (T.pack i) }) "SIM") "Ingest file(s) into the simulation store"
  , Opt.Option "c" ["const"] (Opt.ReqArg (\f o -> o{ optConstFields = (second T.tail $ T.break ('=' ==) $ T.pack f) : optConstFields o }) "FIELD=VALUE") "Field value to add to every ingested record"
  ]

createCatalog :: Catalog -> M String
createCatalog cat@Catalog{ catalogStore = CatalogES{} } = show <$> ES.createIndex cat

main :: IO ()
main = do
  prog <- getProgName
  oargs <- getArgs
  (opts, args) <- case Opt.getOpt Opt.RequireOrder optDescr oargs of
    (foldr ($) def -> o, a, [])
      | null a || isJust (optIngest o) -> return (o, a)
    (_, _, e) -> do
      mapM_ putStrLn e
      putStrLn $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]\n       " ++ prog ++ " [OPTION...] -i SIM FILE[@OFFSET] ...") optDescr
      exitFailure
  conf <- C.load $ optConfig opts
  catalogs <- either throwIO return =<< YAML.decodeFileEither (fromMaybe "catalogs.yml" $ conf C.! "catalogs")
  httpmgr <- HTTP.newManager HTTP.defaultManagerSettings
  es <- ES.initServer (conf C.! "elasticsearch")
  let global = Global
        { globalConfig = conf
        , globalHTTP = httpmgr
        , globalES = es
        , globalCatalogs = catalogs{ catalogMap = HM.filter catalogEnabled $ catalogMap catalogs }
        }

  runGlobal global $ do
    -- create
    mapM_ (liftIO . putStrLn <=< createCatalog . (catalogMap catalogs HM.!)) $ optCreate opts

    -- check catalogs against dbs
    ES.checkIndices

    -- ingest
    forM_ (optIngest opts) $ \sim -> do
      let pconst c [] = return ([], c)
          pconst c ((n,s):r) = do
            (f, c') <- maybe (fail $ "Unknown field: " ++ show n) return $ takeCatalogField n c
            v <- maybe (fail $ "Invalid value: " ++ show s) return $ parseFieldValue f s
            first (v:) <$> pconst c' r
      (consts, cat) <- pconst (catalogMap catalogs HM.! sim) $ optConstFields opts
      forM_ args $ \f -> do
        liftIO $ putStrLn f
        n <- ingest cat consts f
        liftIO $ print n
      ES.flushIndex cat

  when (null (optCreate opts) && isNothing (optIngest opts)) $
    runWaimwork conf $ runGlobal global
      . routeWaiError (\s h _ -> return $ response s h ()) routes
