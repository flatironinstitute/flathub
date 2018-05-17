{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Exception (throwIO)
import           Control.Monad ((<=<), forM_, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Default (Default(def))
import           Data.Function (fix)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe, isNothing, isJust)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Yaml as YAML
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Header (hContentType, hContentDisposition)
import           Network.HTTP.Types.Status (ok200, badRequest400, notFound404)
import qualified Network.Wai as Wai
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import qualified Text.Blaze.Html5 as H hiding (text, textValue)
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Read (readMaybe)
import qualified Waimwork.Blaze as H (text, textValue, preEscapedBuilder)
import qualified Waimwork.Blaze as WH
import qualified Waimwork.Config as C
#ifdef HAVE_pgsql
import qualified Waimwork.Database.PostgreSQL as PG
#endif
import           Waimwork.HTTP (quoteHTTP)
import           Waimwork.Response (response, okResponse)
import           Waimwork.Result (result)
import           Waimwork.Warp (runWaimwork)
import qualified Web.Route.Invertible as R
import           Web.Route.Invertible.URI (routeActionURI)
import           Web.Route.Invertible.Wai (routeWaiError)

import Schema
import Global
import JSON
import CSV
import qualified ES
#ifdef HAVE_pgsql
import qualified PG
#endif
import Static
import Ingest

-- header :: Wai.Request -> HeaderName -> Maybe BS.ByteString
-- header q h = lookup h $ Wai.requestHeaders q

html :: Wai.Request -> H.Markup -> Wai.Response
html req h = okResponse [] $ H.docTypeHtml $ do
  H.head $ do
    forM_ ([["jspm_packages", if isdev then "system.src.js" else "system.js"], ["jspm.config.js"]] ++ if isdev then [["dev.js"]] else [["index.js"]]) $ \src ->
      H.script H.! HA.type_ "text/javascript" H.! HA.src (staticURI src) $ mempty
    -- TODO: use System.resolve:
    forM_ [["jspm_packages", "npm", "datatables.net-dt@1.10.16", "css", "jquery.dataTables.css"], ["main.css"]] $ \src ->
      H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href (staticURI src)
    H.script H.! HA.type_ "text/javascript" H.! HA.src "//cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3/MathJax.js?config=TeX-AMS_CHTML" $ mempty
  H.body h
  where
  isdev = any ((==) "dev" . fst) $ Wai.queryString req

top :: Route ()
top = getPath R.unit $ \() req -> do
  cats <- asks globalCatalogs
  return $ html req $
    H.dl $
      forM_ (HM.toList cats) $ \(sim, cat) -> do
        H.dt $ H.a H.! HA.href (WH.routeActionValue simulation sim mempty) $
          H.text $ catalogTitle cat
        mapM_ (H.dd . H.preEscapedText) $ catalogDescr cat

askCatalog :: Simulation -> M Catalog
askCatalog sim = maybe
  (result $ response notFound404 [] ("No such simulation" :: String))
  return =<< asks (HM.lookup sim . globalCatalogs)

simulation :: Route Simulation
simulation = getPath R.parameter $ \sim req -> do
  cat <- askCatalog sim
  let 
    (qmeth, quri) = routeActionURI catalog sim
    (_, csvuri) = routeActionURI catalogCSV sim
    fields = catalogFieldGroups cat
    fields' = catalogFields cat
    jcat = J.pairs $
         "query" .=*
        (  "method" J..= (BSC.unpack <$> R.fromMethod qmeth)
        <> "uri" J..= show quri
        <> "csv" J..= show csvuri)
      <> "fields" J..= fields'
    fieldBody :: Word -> FieldGroup -> H.Html
    fieldBody d f = H.span WH.!? (HA.title . H.textValue <$> fieldDescr f) $ do
      H.text $ fieldTitle f
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
        H.span H.! HA.id ("hide-" <> H.textValue (fieldName f')) H.! HA.class_ "hide" $ H.preEscapedString "&times;"
        fieldBody d f
    field _ _ f@Field{ fieldSub = Just s } = do
      H.th
          H.! HA.colspan (H.toValue $ length $ expandFields s) $
        fieldBody 1 f
    row :: Word -> [(FieldGroup -> FieldGroup, FieldGroup)] -> H.Html
    row d l = do
      H.tr $ mapM_ (\(p, f) -> field d (p f) f) l
      when (d > 1) $ row (pred d) $ foldMap (\(p, f) -> foldMap (fmap (p . subField f, ) . V.toList) $ fieldSub f) l
  return $ html req $ do
    H.script $ do
      "Catalog="
      H.preEscapedBuilder $ J.fromEncoding jcat
    H.h2 $ H.text $ catalogTitle cat
    mapM_ (H.div . H.preEscapedText) $ catalogDescr cat
    H.div $ "Query and explore a subset using the filters, download your selection using the link below, or get the full dataset above."
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

parseQuery :: Wai.Request -> Query
parseQuery = foldMap parseQueryItem . Wai.queryString where
  parseQueryItem ("offset", Just (readMaybe . BSC.unpack -> Just n)) =
    mempty{ queryOffset = n }
  parseQueryItem ("limit",  Just (readMaybe . BSC.unpack -> Just n)) =
    mempty{ queryLimit  = n }
  parseQueryItem ("sort",   Just s) =
    mempty{ querySort = map ps (BSC.splitWith delim s) } where
    ps f = case BSC.uncons f of
      Just ('+', r) -> (TE.decodeUtf8 r, True)
      Just ('-', r) -> (TE.decodeUtf8 r, False)
      _             -> (TE.decodeUtf8 f, True)
  parseQueryItem ("fields", Just s) =
    mempty{ queryFields = map TE.decodeUtf8 (BSC.splitWith delim s) }
  parseQueryItem ("aggs",   Just s) =
    mempty{ queryAggs = map TE.decodeUtf8 (BSC.splitWith delim s) }
  parseQueryItem ("hist",   Just (BSC.break (':' ==) -> (f, (BSC.uncons -> Just (':', i))))) =
    mempty{ queryHist = Just (TE.decodeUtf8 f, i) }
  parseQueryItem (f,        s) =
    mempty{ queryFilter = [(TE.decodeUtf8 f, a, snd <$> BS.uncons b)] } where
    (a, b) = BSC.break (',' ==) $ fromMaybe BS.empty s
  delim ',' = True
  delim ' ' = True
  delim _ = False

catalog :: Route Simulation
catalog = getPath (R.parameter R.>* "catalog") $ \sim req -> do
  cat <- askCatalog sim
  let query = fillQuery cat $ parseQuery req
  unless (queryLimit query <= 100) $
    result $ response badRequest400 [] ("limit too large" :: String)
  case catalogStore cat of
    CatalogES{} -> do
      res <- ES.queryIndex cat query
      return $ okResponse [] $ clean res
#ifdef HAVE_pgsql
    CatalogPG{} -> do
      res <- PG.queryTable cat query
      return $ okResponse [] res
#endif
  where
  clean = mapObject $ HM.mapMaybeWithKey cleanTop
  cleanTop "aggregations" = Just
  cleanTop "hits" = Just . mapObject (HM.mapMaybeWithKey cleanHits)
  cleanTop _ = const Nothing
  cleanHits "total" = Just
  cleanHits "hits" = Just . mapArray (V.map sourceOnly)
  cleanHits _ = const Nothing
  sourceOnly (J.Object o) = HM.lookupDefault J.emptyObject "_source" o
  sourceOnly v = v
  mapObject :: (J.Object -> J.Object) -> J.Value -> J.Value
  mapObject f (J.Object o) = J.Object (f o)
  mapObject _ v = v
  mapArray :: (V.Vector J.Value -> V.Vector J.Value) -> J.Value -> J.Value
  mapArray f (J.Array v) = J.Array (f v)
  mapArray _ v = v

catalogCSV :: Route Simulation
catalogCSV = getPath (R.parameter R.>* "csv") $ \sim req -> do
  cat <- askCatalog sim
  let query = fillQuery cat $ parseQuery req
  unless (queryOffset query == 0 && null (queryAggs query) && isNothing (queryHist query)) $
    result $ response badRequest400 [] ("offset,aggs not supported for CSV" :: String)
#ifdef HAVE_pgsql
  glob <- ask
#endif
  nextes <- ES.queryBulk cat query
  return $ Wai.responseStream ok200
    [ (hContentType, "text/csv")
    , (hContentDisposition, "attachment; filename=" <> quoteHTTP (TE.encodeUtf8 sim <> ".csv"))
    ] $ \chunk flush -> do
    chunk $ csvTextRow $ queryFields query
    case catalogStore cat of
      CatalogES{} -> fix $ \loop -> do
        block <- nextes
        unless (V.null block) $ do
          chunk $ foldMap csvJSONRow block
          flush
          loop
#ifdef HAVE_pgsql
      CatalogPG{} -> runGlobal glob $ PG.queryBulk cat query $ \nextpg -> fix $ \loop -> do
        block <- nextpg
        unless (null block) $ do
          chunk $ foldMap csvJSONRow block
          flush
          loop
#endif


routes :: R.RouteMap Action
routes = R.routes
  [ R.routeNormCase top
  , R.routeNormCase static
  , R.routeNormCase simulation
  , R.routeNormCase catalog
  , R.routeNormCase catalogCSV
  ]

data Opts = Opts
  { optConfig :: FilePath
  , optCreate :: [Simulation]
  , optIngest :: Maybe Simulation
  }

instance Default Opts where
  def = Opts "config" [] Nothing

optDescr :: [Opt.OptDescr (Opts -> Opts)]
optDescr =
  [ Opt.Option "f" ["config"] (Opt.ReqArg (\c o -> o{ optConfig = c }) "FILE") "Configuration file [config]"
  , Opt.Option "s" ["create"] (Opt.ReqArg (\i o -> o{ optCreate = T.pack i : optCreate o }) "SIM") "Create storage schema for the simulation"
  , Opt.Option "i" ["ingest"] (Opt.ReqArg (\i o -> o{ optIngest = Just (T.pack i) }) "SIM") "Ingest file(s) into the simulation store"
  ]

createCatalog :: Catalog -> M String
createCatalog cat@Catalog{ catalogStore = CatalogES{} } = show <$> ES.createIndex cat
#ifdef HAVE_pgsql
createCatalog cat@Catalog{ catalogStore = CatalogPG{} } = show <$> PG.createTable cat
#endif

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
#ifdef HAVE_pgsql
  pg <- PG.initDB (conf C.! "postgresql")
#endif
  let global = Global
        { globalConfig = conf
        , globalHTTP = httpmgr
        , globalES = es
#ifdef HAVE_pgsql
        , globalPG = pg
#endif
        , globalCatalogs = catalogs
        }

  runGlobal global $ do
    -- create
    mapM_ (liftIO . putStrLn <=< createCatalog . (catalogs HM.!)) $ optCreate opts

    -- check catalogs against dbs
    ES.checkIndices
#ifdef HAVE_pgsql
    PG.checkTables
#endif

    -- ingest
    forM_ (optIngest opts) $ \sim -> do
      let cat = catalogs HM.! sim
      forM_ args $ \f -> do
        liftIO $ putStrLn f
        n <- ingest cat f
        liftIO $ print n
      ES.flushIndex cat

  when (null (optCreate opts) && isNothing (optIngest opts)) $
    runWaimwork conf $ runGlobal global
      . routeWaiError (\s h _ -> return $ response s h ()) routes
