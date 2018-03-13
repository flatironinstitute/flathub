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
import           Data.Foldable (fold)
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
import           System.Exit (exitFailure)
import qualified System.FilePath as FP
import qualified Text.Blaze.Html5 as H hiding (text, textValue)
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Read (readMaybe)
import qualified Waimwork.Blaze as H hiding ((!?))
import qualified Waimwork.Config as C
import           Waimwork.Response (response, okResponse)
import           Waimwork.Result (result)
import           Waimwork.Warp (runWaimwork)
import qualified Web.Route.Invertible as R
import           Web.Route.Invertible.URI (routeActionURI)
import           Web.Route.Invertible.Wai (routeWaiError)

import qualified ES
import Schema
import Global
import JSON
import CSV

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
    H.table H.! HA.id "tcat" H.! HA.class_ "compact" $ do
      H.thead $ row (fieldsDepth fields) fields
      H.tfoot $ H.tr $ H.td H.! HA.colspan (H.toValue $ length fields') H.! HA.class_ "loading" $ "loading..."
  where
  dtype ES.Long = "num"
  dtype ES.Integer = "num"
  dtype ES.Short = "num"
  dtype ES.Byte = "num"
  dtype ES.Double = "num"
  dtype ES.Float = "num"
  dtype ES.HalfFloat = "num"
  dtype (ES.ScaledFloat _) = "num"
  dtype ES.Date = "date"
  dtype _ = "string"

parseQuery :: Wai.Request -> ES.Query
parseQuery = foldMap parseQueryItem . Wai.queryString where
  parseQueryItem ("offset", Just (readMaybe . BSC.unpack -> Just n)) = mempty{ ES.queryOffset = n }
  parseQueryItem ("limit",  Just (readMaybe . BSC.unpack -> Just n)) = mempty{ ES.queryLimit  = n }
  parseQueryItem ("sort",   Just s) = mempty{ ES.querySort = map ps (BSC.split ',' s) } where
    ps f = case BSC.uncons f of
      Just ('+', r) -> (TE.decodeUtf8 r, True)
      Just ('-', r) -> (TE.decodeUtf8 r, False)
      _             -> (TE.decodeUtf8 f, True)
  parseQueryItem ("fields", Just s) = mempty{ ES.queryFields = map TE.decodeUtf8 (BSC.split ',' s) }
  parseQueryItem ("aggs",   Just s) = mempty{ ES.queryAggs = map TE.decodeUtf8 (BSC.split ',' s) }
  parseQueryItem ("hist",   Just s) = mempty{ ES.queryHist = Just (TE.decodeUtf8 s) }
  parseQueryItem (f,        s) = mempty{ ES.queryFilter = [(TE.decodeUtf8 f, a, snd <$> BS.uncons b)] } where
    (a, b) = BSC.break (',' ==) $ fromMaybe BS.empty s

catalog :: Route Simulation
catalog = getPath (R.parameter R.>* "catalog") $ \sim req -> do
  cat <- askCatalog sim
  let query = parseQuery req
  unless (ES.queryLimit query > 0 && ES.queryLimit query <= 100) $
    result $ response badRequest400 [] ("limit too large" :: String)
  res <- ES.queryIndex cat query
  return $ okResponse [(hContentType, "application/json")] $ clean res
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
  let query = parseQuery req
      fields = if null (ES.queryFields query) then map fieldName $ expandFields $ catalogFields cat else ES.queryFields query
      parse = J.withObject "query" $ \q -> (,)
        <$> q J..: "_scroll_id"
        <*> parseJSONField "hits" (J.withObject "hits" $
          parseJSONField "hits" $ J.withArray "hits" $
            V.mapM $ J.withObject "hit" $
              parseJSONField "_source" $ J.withObject "source" $ \d ->
                return $ csvJSONRow $ map (\f -> HM.lookupDefault J.Null f d) fields)
          q
  unless (ES.queryLimit query == 0 && ES.queryOffset query == 0 && null (ES.queryAggs query) && isNothing (ES.queryHist query)) $
    result $ response badRequest400 [] ("limit,offset,aggs not supported for CSV" :: String)
  glob <- ask
  return $ Wai.responseStream ok200 [(hContentType, "text/csv")] $ \chunk flush -> do
    chunk $ csvTextRow fields
    let
      send b = chunk b >> flush
      loop res = do
        (sid, csv) <- either fail return $ J.parseEither parse res
        unless (null csv) $ do
          () <- liftIO $ send $ fold csv
          loop =<< ES.scrollSearch sid
    runGlobal glob $ loop =<< ES.queryIndex cat query{ ES.queryScroll = True }

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
  , optIndices :: [Simulation]
  }

instance Default Opts where
  def = Opts "config" []

optDescr :: [Opt.OptDescr (Opts -> Opts)]
optDescr =
  [ Opt.Option "c" ["config"] (Opt.ReqArg (\c o -> o{ optConfig = c }) "FILE") "Configuration file [config]"
  , Opt.Option "i" ["index"] (Opt.ReqArg (\i o -> o{ optIndices = read i : optIndices o }) "SIM") "Create an ES index for the given simulation"
  ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  opts <- case Opt.getOpt Opt.Permute optDescr args of
    (f, [], []) -> return $ foldr ($) def f
    (_, _, e) -> do
      mapM_ putStrLn e
      putStrLn $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]") optDescr
      exitFailure
  conf <- C.load $ optConfig opts
  catalogs <- either throwIO return =<< YAML.decodeFileEither (fromMaybe "catalogs.yml" $ conf C.! "catalogs")
  httpmgr <- HTTP.newManager HTTP.defaultManagerSettings
  es <- ES.initServer (conf C.! "elasticsearch")
  let global = Global
        { globalConfig = conf
        , globalHTTP = httpmgr
        , globalES = es
        , globalCatalogs = catalogs
        }

  runGlobal global $ mapM_ (liftIO . print <=< ES.createIndex def . (catalogs HM.!)) $ optIndices opts
    
  -- check catalogs against es
  indices <- runGlobal global $ ES.getIndices $ map (T.unpack . catalogIndex) $ HM.elems catalogs
  either
    (fail . ("ES index mismatch: " ++))
    return
    $ J.parseEither (checkESIndices $ HM.elems catalogs) indices

  runWaimwork conf $ runGlobal global
    . routeWaiError (\s h _ -> return $ response s h ()) routes
