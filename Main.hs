{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Arrow (first)
import           Control.Monad (forM_, guard, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import           Data.Array ((!), array, range)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Tuple (swap)
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.Status (ok200, badRequest400)
import           Network.Mime (defaultMimeLookup)
import qualified Network.Wai as Wai
import qualified System.FilePath as FP
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Read (readMaybe)
import qualified Waimwork.Blaze as H
import qualified Waimwork.Config as C
import           Waimwork.Response (response, okResponse)
import           Waimwork.Result (result)
import           Waimwork.Static (staticFileResponse)
import           Waimwork.Warp (runWaimwork)
import qualified Web.Route.Invertible as R
import           Web.Route.Invertible.URI (routeActionURI)
import           Web.Route.Invertible.Wai (routeWaiError)

import qualified ES
import Global
import JSON
import CSV

getPath :: R.Path p -> (p -> Action) -> R.RouteAction p Action
getPath p = R.RouteAction $ R.routeMethod R.GET R.*< R.routePath p

-- header :: Wai.Request -> HeaderName -> Maybe BS.ByteString
-- header q h = lookup h $ Wai.requestHeaders q

staticURI :: [FilePathComponent] -> H.AttributeValue
staticURI p = H.routeActionValue static p mempty

html :: H.Markup -> Wai.Response
html h = okResponse [] $ H.docTypeHtml $ do
  H.head $ do
    H.script H.! HA.src (staticURI ["jspm_packages", "system.src.js"]) $ mempty
    H.script H.! HA.src (staticURI ["jspm.config.js"]) $ mempty
    H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css"
      H.! HA.href (staticURI ["jspm_packages", "npm", "datatables.net-dt@1.10.16", "css", "jquery.dataTables.css"])
  H.body h

top :: Route ()
top = getPath R.unit $ \() _ -> return $ html $ do
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

static :: Route [FilePathComponent]
static = getPath ("js" R.*< R.manyI R.parameter) $ \paths req -> do
  let path = FP.joinPath ("js" : map componentFilePath paths)
  liftIO $ staticFileResponse req
    [ (hContentType, defaultMimeLookup (T.pack path))
    ] path

fixedFields :: Simulation -> [T.Text]
fixedFields IllustrisGroup = ["simulation", "snapshot"]
fixedFields Neutrino = ["simulation", "instance", "snapshot"]

askIndex :: Simulation -> M ES.Index
askIndex sim = asks $ (! sim) . globalIndices

simulation :: Route Simulation
simulation = getPath R.parameter $ \sim _ -> do
  idx <- askIndex sim
  let 
    (qmeth, quri) = routeActionURI catalog sim
    cat = J.pairs $
         "name" J..= show sim
      <> "query" `JE.pair` J.pairs ("method" J..= (BSC.unpack <$> R.fromMethod qmeth) <> "uri" J..= show quri)
      <> "props" J..= ES.indexMapping idx
      <> "fixed" J..= fixedFields sim
  return $ html $ do
    H.h2 $ H.string $ show sim
    H.table H.! HA.id "filt" $ mempty
    H.table H.! HA.id "tcat" H.! HA.class_ "compact" $ mempty
    H.script $ do
      "Catalog = "
      H.unsafeBuilder $ J.fromEncoding cat
      ";System.import('main')"

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
  parseQueryItem (f,        s) = mempty{ ES.queryFilter = [(TE.decodeUtf8 f, a, snd <$> BS.uncons b)] } where
    (a, b) = BSC.break (',' ==) $ fromMaybe BS.empty s

chunkSize :: Word
chunkSize = 100

catalog :: Route Simulation
catalog = getPath (R.parameter R.>* "catalog") $ \sim req -> do
  idx <- askIndex sim
  let query = parseQuery req
  when (ES.queryLimit query > chunkSize) $ result $ response badRequest400 [] ("limit too large" :: String)
  res <- ES.queryIndex (show sim) idx query
  return $ okResponse [(hContentType, "application/json")] res

catalogCSV :: Route Simulation
catalogCSV = getPath (R.parameter R.>* "csv") $ \sim req -> do
  idx <- askIndex sim
  let query = parseQuery req
      fields = if null (ES.queryFields query) then HM.keys (ES.indexMapping idx) else ES.queryFields query
      parse = J.withObject "query" $ parseJSONField "hits" $
        J.withObject "hits" $ parseJSONField "hits" $
          J.withArray "hits" $ V.mapM $ J.withObject "hit" $ \d ->
            return $ csvJSONRow $ map (\f -> HM.lookupDefault J.Null f d) fields
      loop send q = do
        let q' = q{ ES.queryLimit = min chunkSize (ES.queryLimit q) }
        res <- ES.queryIndex (show sim) idx q'
        csv <- either fail return $ J.parseEither parse res
        unless (null csv) $ do
          () <- liftIO $ send $ fold csv
          when (ES.queryLimit q' < ES.queryLimit q) $
            loop send q
              { ES.queryOffset = ES.queryOffset q + ES.queryLimit q'
              , ES.queryLimit  = ES.queryLimit  q - ES.queryLimit q'
              }
  unless (null (ES.queryAggs query)) $ result $ response badRequest400 [] ("aggs not supported for CSV" :: String)
  glob <- ask
  return $ Wai.responseStream ok200 [(hContentType, "text/csv")] $ \chunk flush -> do
    chunk $ csvTextRow fields
    runGlobal glob $ loop (\b -> chunk b >> flush) query

routes :: R.RouteMap Action
routes = R.routes
  [ R.routeNormCase top
  , R.routeNormCase static
  , R.routeNormCase simulation
  , R.routeNormCase catalog
  , R.routeNormCase catalogCSV
  ]

main :: IO ()
main = do
  conf <- C.load "config"
  httpmgr <- HTTP.newManager HTTP.defaultManagerSettings
  es <- ES.initServer (conf C.! "elasticsearch")
  let simrange = (minBound, maxBound)
      global' = Global
        { globalConfig = conf
        , globalHTTP = httpmgr
        , globalES = es
        , globalIndices = array (swap simrange) []
        }
  indices <- runGlobal global' $ ES.getIndices $ map show (range simrange)
  let global = global'
        { globalIndices = array simrange $ map (first $ read . T.unpack) $ HM.toList indices
        }
  runWaimwork conf $ runGlobal global
    . routeWaiError (\s h _ -> return $ response s h ()) routes
