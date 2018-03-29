{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Exception (throwIO)
import           Control.Monad ((<=<), forM_, guard, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Default (Default(def))
import           Data.Function (fix)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Monoid ((<>))
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Yaml as YAML
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Header (hContentType, hCacheControl)
import           Network.HTTP.Types.Status (ok200, badRequest400)
import qualified Network.Mime as Mime
import qualified Network.Wai as Wai
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitSuccess, exitFailure)
import qualified System.FilePath as FP
import qualified Text.Blaze.Html5 as H hiding (text, textValue)
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Read (readMaybe)
import qualified Waimwork.Blaze as H hiding ((!?))
import qualified Waimwork.Config as C
import qualified Waimwork.Database.PostgreSQL as PG
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
import qualified PG
import Ingest

getPath :: R.Path p -> (p -> Action) -> R.RouteAction p Action
getPath p = R.RouteAction $ R.routeMethod R.GET R.*< R.routePath p

-- header :: Wai.Request -> HeaderName -> Maybe BS.ByteString
-- header q h = lookup h $ Wai.requestHeaders q

staticURI :: [FilePathComponent] -> H.AttributeValue
staticURI p = H.routeActionValue static p mempty

html :: Wai.Request -> H.Markup -> Wai.Response
html req h = okResponse [] $ H.docTypeHtml $ do
  H.head $ do
    forM_ ([["jspm_packages", "system.src.js"], ["jspm.config.js"]] ++ if isdev then [["dev.js"]] else [["index.js"]]) $ \src ->
      H.script H.! HA.type_ "text/javascript" H.! HA.src (staticURI src) $ mempty
    -- TODO: use System.resolve:
    forM_ [["jspm_packages", "npm", "datatables.net-dt@1.10.16", "css", "jquery.dataTables.css"], ["main.css"]] $ \src ->
      H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href (staticURI src)
    H.script H.! HA.type_ "text/javascript" H.! HA.src "//cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3/MathJax.js?config=TeX-AMS_CHTML" $ mempty
  H.body h
  where
  isdev = any ((==) "dev" . fst) $ Wai.queryString req

top :: Route ()
top = getPath R.unit $ \() req -> return $ html req $ do
  H.ul $
    forM_ (enumFromTo minBound maxBound) $ \sim ->
      H.li $ H.a H.! HA.href (H.routeActionValue simulation sim mempty) $ H.string $
        show sim

newtype FilePathComponent = FilePathComponent{ componentFilePath :: String }
  deriving (IsString)

instance R.Parameter R.PathString FilePathComponent where
  parseParameter p = do
    s <- R.parseParameter p
    guard $ FP.isValid s && head s /= '.' && not (any FP.isPathSeparator s)
    return $ FilePathComponent s
  renderParameter (FilePathComponent s) = R.renderParameter s

getMimeType :: Mime.FileName -> Mime.MimeType
getMimeType = Mime.mimeByExt (Map.insert "ts" "text/typescript" Mime.defaultMimeMap) Mime.defaultMimeType

static :: Route [FilePathComponent]
static = getPath ("js" R.*< R.manyI R.parameter) $ \paths _ -> do
  let path = FP.joinPath ("js" : map componentFilePath paths)
  return $ Wai.responseFile ok200
    [ (hContentType, getMimeType (T.pack path))
    , (hCacheControl, "public, max-age=" <> (if length paths == 1 then "10, must-revalidate" else "1000000"))
    ] path Nothing

askCatalog :: Simulation -> M Catalog
askCatalog sim = asks $ (HM.! sim) . globalCatalogs

simulation :: Route Simulation
simulation = getPath R.parameter $ \sim req -> do
  cat <- askCatalog sim
  let 
    (qmeth, quri) = routeActionURI catalog sim
    (_, csvuri) = routeActionURI catalogCSV sim
    fields = catalogFields cat
    fields' = expandFields fields
    jcat = J.pairs $
         "title" J..= catalogTitle cat
      <> "query" .=*
        (  "method" J..= (BSC.unpack <$> R.fromMethod qmeth)
        <> "uri" J..= show quri
        <> "csv" J..= show csvuri)
      <> "fields" J..= fields'
    fieldBody f = H.span H.! HA.title (H.textValue $ fieldDescr f) $ H.text $ fieldTitle f
    field :: Word -> FieldGroup -> H.Html
    field d f@Field{ fieldSub = Nothing } = do
      H.th
          H.! HA.rowspan (H.toValue d)
          H.! H.dataAttribute "data" (H.textValue $ fieldName f)
          H.! H.dataAttribute "name" (H.textValue $ fieldName f) 
          H.! H.dataAttribute "type" (dtype $ fieldType f)
          H.!? (not (fieldDisp f), H.dataAttribute "visible" "false")
          H.! H.dataAttribute "default-content" mempty $ do
        H.span H.! HA.id ("hide-" <> H.textValue (fieldName f)) H.! HA.class_ "hide" $ H.preEscapedString "&times;"
        fieldBody f
    field _ f@Field{ fieldSub = Just s } = do
      H.th
          H.! HA.colspan (H.toValue $ length $ expandFields s) $
        fieldBody f
    row :: Word -> FieldGroups -> H.Html
    row d l = do
      H.tr $ mapM_ (field d) l
      when (d > 1) $ row (pred d) $ foldMap (\f -> foldMap (subField f <$>) $ fieldSub f) l
  return $ html req $ do
    H.script $ do
      "Catalog="
      H.preEscapedBuilder $ J.fromEncoding jcat
    H.h2 $ H.string $ show sim
    H.table H.! HA.id "filt" $ mempty
    H.div H.! HA.id "dhist" $
      H.canvas H.! HA.id "hist" $ mempty
    H.table H.! HA.id "tcat" H.! HA.class_ "compact" $ do
      H.thead $ row (fieldsDepth fields) fields
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
    mempty{ querySort = map ps (BSC.split ',' s) } where
    ps f = case BSC.uncons f of
      Just ('+', r) -> (TE.decodeUtf8 r, True)
      Just ('-', r) -> (TE.decodeUtf8 r, False)
      _             -> (TE.decodeUtf8 f, True)
  parseQueryItem ("fields", Just s) =
    mempty{ queryFields = map TE.decodeUtf8 (BSC.split ',' s) }
  parseQueryItem ("aggs",   Just s) =
    mempty{ queryAggs = map TE.decodeUtf8 (BSC.split ',' s) }
  parseQueryItem ("hist",   Just (BSC.break (':' ==) -> (f, (BSC.uncons -> Just (':', i))))) =
    mempty{ queryHist = Just (TE.decodeUtf8 f, i) }
  parseQueryItem (f,        s) =
    mempty{ queryFilter = [(TE.decodeUtf8 f, a, snd <$> BS.uncons b)] } where
    (a, b) = BSC.break (',' ==) $ fromMaybe BS.empty s

catalog :: Route Simulation
catalog = getPath (R.parameter R.>* "catalog") $ \sim req -> do
  cat <- askCatalog sim
  let query = fillQuery cat $ parseQuery req
  unless (queryLimit query > 0 && queryLimit query <= 100) $
    result $ response badRequest400 [] ("limit too large" :: String)
  case catalogStore cat of
    CatalogES{} -> do
      res <- ES.queryIndex cat query
      return $ okResponse [] $ clean res
    CatalogPG{} -> do
      res <- PG.queryTable cat query
      return $ okResponse [] res
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
  unless (queryLimit query == 0 && queryOffset query == 0 && null (queryAggs query) && isNothing (queryHist query)) $
    result $ response badRequest400 [] ("limit,offset,aggs not supported for CSV" :: String)
  glob <- ask
  nextes <- ES.queryBulk cat query
  return $ Wai.responseStream ok200 [(hContentType, "text/csv")] $ \chunk flush -> do
    chunk $ csvTextRow $ queryFields query
    case catalogStore cat of
      CatalogES{} -> fix $ \loop -> do
        block <- nextes
        unless (V.null block) $ do
          chunk $ foldMap csvJSONRow block
          flush
          loop
      CatalogPG{} -> runGlobal glob $ PG.queryBulk cat query $ \nextpg -> fix $ \loop -> do
        block <- nextpg
        unless (null block) $ do
          chunk $ foldMap csvJSONRow block
          flush
          loop


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
  , Opt.Option "s" ["create"] (Opt.ReqArg (\i o -> o{ optCreate = read i : optCreate o }) "SIM") "Create storage schema for the simulation"
  , Opt.Option "i" ["ingest"] (Opt.ReqArg (\i o -> o{ optIngest = Just (read i) }) "SIM") "Ingest file(s) into the simulation store"
  ]

createCatalog :: Catalog -> M String
createCatalog cat@Catalog{ catalogStore = CatalogES{} } = show <$> ES.createIndex cat
createCatalog cat@Catalog{ catalogStore = CatalogPG{} } = show <$> PG.createTable cat

main :: IO ()
main = do
  prog <- getProgName
  oargs <- getArgs
  (opts, args) <- case Opt.getOpt Opt.RequireOrder optDescr oargs of
    (f, a, []) -> return (foldr ($) def f, a)
    (_, _, e) -> do
      mapM_ putStrLn e
      putStrLn $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]") optDescr
      exitFailure
  conf <- C.load $ optConfig opts
  catalogs <- either throwIO return =<< YAML.decodeFileEither (fromMaybe "catalogs.yml" $ conf C.! "catalogs")
  httpmgr <- HTTP.newManager HTTP.defaultManagerSettings
  es <- ES.initServer (conf C.! "elasticsearch")
  pg <- PG.initDB (conf C.! "postgresql")
  let global = Global
        { globalConfig = conf
        , globalHTTP = httpmgr
        , globalES = es
        , globalPG = pg
        , globalCatalogs = catalogs
        }

  runGlobal global $ do
    -- create
    mapM_ (liftIO . putStrLn <=< createCatalog . (catalogs HM.!)) $ optCreate opts

    -- check catalogs against dbs
    ES.checkIndices >> PG.checkTables

    -- ingest
    forM_ (optIngest opts) $ \sim -> do
      let cat = catalogs HM.! sim
      forM_ args $ \f -> do
        liftIO $ putStrLn f
        n <- ingestHDF5 cat f
        liftIO $ print n
      liftIO exitSuccess

  runWaimwork conf $ runGlobal global
    . routeWaiError (\s h _ -> return $ response s h ()) routes
