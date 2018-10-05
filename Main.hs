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
import Html

routes :: R.RouteMap Action
routes = R.routes
  [ R.routeNormCase topPage
  , R.routeNormCase static
  , R.routeNormCase staticHtml
  , R.routeNormCase simulationPage
  , R.routeNormCase comparePage
  , R.routeNormCase catalog
  , R.routeNormCase catalogBulk
  , R.routeNormCase sqlSchema
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
        , globalDevMode = fromMaybe False $ conf C.! "dev"
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
