{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Arrow (first, second)
import           Control.Monad ((<=<), forM_, when, unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default (Default(def))
import qualified Data.HashMap.Strict as HM
import           Data.Foldable (foldrM)
import           Data.Maybe (fromMaybe, isNothing, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.IO (hPutStrLn, stderr)
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
import Html
import JSON
import Api
import Backend
import qualified KeyedMap as KM

routes :: R.RouteMap Action
routes = R.routes (
  [ R.routeNormCase topPage
  , R.routeNormCase static
  , R.routeNormCase staticHtml
  , R.routeNormCase agoraPage
  , R.routeNormCase firePage
  , R.routeNormCase catalogPage
  , R.routeNormCase groupPage
  , R.routeNormCase comparePage
  , R.routeNormCase catalog
  , R.routeNormCase catalogBulk
  , R.routeNormCase sqlSchema
  , R.routeNormCase csvSchema
  , R.routeNormCase attachment
  , R.routeNormCase openApi
  ] ++ apiRoutes)

data Opts = Opts
  { optConfig :: FilePath
  , optCreate
  , optClose
  , optOpen :: [Simulation]
  , optIngest :: Maybe Simulation
  , optConstFields :: [(T.Text, T.Text)]
  , optStats :: [Maybe Simulation]
  }

instance Default Opts where
  def = Opts "config" [] [] [] Nothing [] []

optDescr :: [Opt.OptDescr (Opts -> Opts)]
optDescr =
  [ Opt.Option "f" ["config"] (Opt.ReqArg (\c o -> o{ optConfig = c }) "FILE") "Configuration file [config]"
  , Opt.Option "s" ["create"] (Opt.ReqArg (\i o -> o{ optCreate = T.pack i : optCreate o }) "CAT") "Create storage schema for the catalog"
  , Opt.Option "e" ["close" ] (Opt.ReqArg (\i o -> o{ optClose  = T.pack i : optClose o  }) "CAT") "Finalize (flush and make read-only) catalog storage"
  , Opt.Option "o" ["open"  ] (Opt.ReqArg (\i o -> o{ optOpen   = T.pack i : optOpen o   }) "CAT") "Re-open catalog storage"
  , Opt.Option "i" ["ingest"] (Opt.ReqArg (\i o -> o{ optIngest = Just (T.pack i) }) "CAT") "Ingest file(s) into the catalog store"
  , Opt.Option "c" ["const" ] (Opt.ReqArg (\f o -> o{ optConstFields = (second T.tail $ T.break ('=' ==) $ T.pack f) : optConstFields o }) "FIELD=VALUE") "Field value to add to every ingested record"
  , Opt.Option "u" ["stats" ] (Opt.OptArg (\i o -> o{ optStats = (T.pack <$> i) : optStats o }) "CAT") "Update catalog/_stats.yml for CAT [or all missing]"
  ]

createCatalog :: Catalog -> M String
createCatalog cat = show <$> ES.createIndex cat

updateStats :: FilePath -> Bool -> Catalog -> M ()
updateStats file force cat = do
  j <- liftIO $ Y.decodeFileThrow file
  let jc = HM.lookupDefault mempty (catalogName cat) j
  jc' <- addcount =<< foldrM addfield jc fields
  liftIO $ Y.encodeFile file $ HM.insert (catalogName cat) jc' j
  where
  addcount = addval "count" $ queryCount cat filts
  addfield f = addval (fieldName f) $ fieldValue . (HM.! fieldName f) <$> queryStats cat (StatsArgs filts (KM.singleton f))
  addval m f j
    | force || not (HM.member m j) = do
      liftIO $ TIO.putStrLn $ catalogName cat <> ('.' `T.cons` m)
      v <- f
      return $ HM.insert m (Y.toJSON v) j
    | otherwise = return j
  filts = mempty
  fields = HM.filter (\f -> not (or (fieldStore f))) $ catalogFieldMap cat

main :: IO ()
main = do
  prog <- getProgName
  oargs <- getArgs
  (opts, args) <- case Opt.getOpt Opt.RequireOrder optDescr oargs of
    (foldr ($) def -> o, a, [])
      | null a || isJust (optIngest o) -> return (o, a)
    (_, _, e) -> do
      mapM_ (hPutStrLn stderr) e
      putStrLn $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]\n       " ++ prog ++ " [OPTION...] -i CAT FILE[@OFFSET] ...") optDescr
      exitFailure
  conf <- C.load $ optConfig opts
  let catalogpath = fromMaybe "catalogs" $ conf C.! "catalogs"
  catalogs <- loadYamlPath catalogpath
  httpmgr <- HTTP.newManager HTTP.defaultManagerSettings
    { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 300000000
    }
  es <- ES.initServer (conf C.! "elasticsearch")
  let global = Global
        { globalConfig = conf
        , globalES = HTTP.setRequestManager httpmgr es
        , globalCatalogs = catalogs
        , globalDataDir = fromMaybe "." $ conf C.! "datadir"
        , globalDevMode = fromMaybe False $ conf C.! "dev"
        }

  catalogs' <- runGlobal global $ do
    -- create
    mapM_ (liftIO . hPutStrLn stderr <=< createCatalog . (catalogMap catalogs HM.!)) $ optCreate opts

    -- check catalogs against dbs
    errs <- ES.checkIndices
    liftIO $ forM_ (HM.toList errs) $ \(k, e) ->
      hPutStrLn stderr $ T.unpack k ++ ": " ++ e

    -- ingest
    forM_ (optIngest opts) $ \sim -> do
      let pconst c [] = return ([], c)
          pconst c ((n,s):r) = do
            (f, c') <- maybe (fail $ "Unknown field: " ++ show n) return $ takeCatalogField n c
            v <- maybe (fail $ "Invalid value: " ++ show s) return $ parseFieldValue f $ TE.encodeUtf8 s
            first (v:) <$> pconst c' r
      (consts, cat) <- pconst (catalogMap catalogs HM.! sim) $ optConstFields opts
      n <- ingest cat consts args
      liftIO $ print n
      when (n > 0) $ ES.flushIndex cat

    forM_ (optOpen opts) $ \sim -> do
      let cat = catalogMap catalogs HM.! sim
      ES.openIndex cat

    forM_ (optClose opts) $ \sim -> do
      let cat = catalogMap catalogs HM.! sim
      ES.flushIndex cat
      n <- ES.countIndex cat
      liftIO $ print n
      unless (all (n ==) $ catalogCount cat) $ fail $ T.unpack sim ++ ": incorrect document count"
      ES.closeIndex cat

    forM_ (optStats opts) $ let f = catalogpath </> "_stats.yml" in maybe
      (mapM_ (updateStats f False) $ catalogMap catalogs)
      (\sim -> updateStats f True $ catalogMap catalogs HM.! sim)

    return $ pruneCatalogs errs catalogs

  when (null (optCreate opts ++ optOpen opts ++ optClose opts) && null (optStats opts) && isNothing (optIngest opts)) $
    runWaimwork conf $
      runGlobalWai global{ globalCatalogs = catalogs' }
      . routeWaiError (\s h _ -> return $ response s h ()) routes
