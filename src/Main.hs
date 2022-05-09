{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Arrow (first, second)
import           Control.Monad ((<=<), forM_, when, unless, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default (Default(def))
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe, isNothing, isJust)
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
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
import Attach
import JSON
import Backend
import Api

routes :: R.RouteMap Action
routes = R.routes (
  [ R.routeNormCase topPage
  , R.routeNormCase static
  , R.routeNormCase staticHtml
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
  }

instance Default Opts where
  def = Opts "config" [] [] [] Nothing []

optDescr :: [Opt.OptDescr (Opts -> Opts)]
optDescr =
  [ Opt.Option "f" ["config"] (Opt.ReqArg (\c o -> o{ optConfig = c }) "FILE") "Configuration file [config]"
  , Opt.Option "s" ["create"] (Opt.ReqArg (\i o -> o{ optCreate = T.pack i : optCreate o }) "SIM") "Create storage schema for the simulation"
  , Opt.Option "e" ["close" ] (Opt.ReqArg (\i o -> o{ optClose  = T.pack i : optClose o  }) "SIM") "Finalize (flush and make read-only) simulation storage"
  , Opt.Option "o" ["open"  ] (Opt.ReqArg (\i o -> o{ optOpen  = T.pack i : optClose o  }) "SIM") "Re-open simulation storage"
  , Opt.Option "i" ["ingest"] (Opt.ReqArg (\i o -> o{ optIngest = Just (T.pack i) }) "SIM") "Ingest file(s) into the simulation store"
  , Opt.Option "c" ["const"] (Opt.ReqArg (\f o -> o{ optConstFields = (second T.tail $ T.break ('=' ==) $ T.pack f) : optConstFields o }) "FIELD=VALUE") "Field value to add to every ingested record"
  ]

createCatalog :: Catalog -> M String
createCatalog cat = show <$> ES.createIndex cat

populateStats :: Catalog -> M Catalog
populateStats cat = do
  stats <- once $ queryStats cat (StatsArgs mempty $ HM.filter (not . or . fieldStore) $ catalogFieldMap cat)
  return cat{ catalogStats = stats }

main :: IO ()
main = do
  prog <- getProgName
  oargs <- getArgs
  (opts, args) <- case Opt.getOpt Opt.RequireOrder optDescr oargs of
    (foldr ($) def -> o, a, [])
      | null a || isJust (optIngest o) -> return (o, a)
    (_, _, e) -> do
      mapM_ (hPutStrLn stderr) e
      putStrLn $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]\n       " ++ prog ++ " [OPTION...] -i SIM FILE[@OFFSET] ...") optDescr
      exitFailure
  conf <- C.load $ optConfig opts
  catalogs <- loadYamlPath (fromMaybe "catalogs" $ conf C.! "catalogs")
  httpmgr <- HTTP.newManager HTTP.defaultManagerSettings
    { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 300000000
    }
  es <- ES.initServer (conf C.! "elasticsearch")
  let global = Global
        { globalConfig = conf
        , globalHTTP = httpmgr
        , globalES = es
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
            v <- maybe (fail $ "Invalid value: " ++ show s) return $ parseFieldValue f s
            first (v:) <$> pconst c' r
      (consts, cat) <- pconst (catalogMap catalogs HM.! sim) $ optConstFields opts
      n <- ingest cat consts args
      liftIO $ print n
      when (n > 0) $ void $ ES.flushIndex cat

    forM_ (optOpen opts) $ \sim -> do
      let cat = catalogMap catalogs HM.! sim
      liftIO . print =<< ES.openIndex cat

    forM_ (optClose opts) $ \sim -> do
      let cat = catalogMap catalogs HM.! sim
      liftIO . print =<< ES.flushIndex cat
      n <- ES.countIndex cat
      liftIO $ print n
      unless (all (n ==) $ catalogCount cat) $ fail $ T.unpack sim ++ ": incorrect document count"
      liftIO . print =<< ES.closeIndex cat

    let catalogs' = pruneCatalogs errs catalogs
    cats <- mapM populateStats (catalogMap catalogs')
    return $ catalogs'{ catalogMap = cats }

  when (null (optCreate opts ++ optClose opts) && isNothing (optIngest opts)) $
    runWaimwork conf $
      runGlobalWai global{ globalCatalogs = catalogs' }
      . routeWaiError (\s h _ -> return $ response s h ()) routes
