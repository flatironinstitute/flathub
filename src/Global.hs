{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Global
  ( Global(..)
  , M
  , runGlobal
  , runGlobalWai
  , Err
  , runErr
  , raise
  , raise400
  , raise404
  , Action
  , Route
  , Simulation
  , getPath
  , askCatalog
  , once
  ) where

import qualified Data.HashMap.Strict as HM
import           Control.Concurrent.MVar (newMVar, modifyMVar)
import           Control.Monad.Except (ExceptT(..), liftEither, runExcept)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT(..), ask, asks)
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wai as Wai
import qualified Waimwork.Config as C
import           Waimwork.Response (response)
import qualified Web.Route.Invertible as R

import Error
import Catalog

data Global = Global
  { globalConfig :: C.Config
  , globalHTTP :: HTTP.Manager
  , globalES :: HTTP.Request
  , globalCatalogs :: Catalogs
  , globalDataDir :: FilePath
  , globalDevMode :: Bool
  }

type M = ErrT (ReaderT Global IO)

runGlobal :: Global -> M a -> IO a
runGlobal g (ExceptT (ReaderT f)) = either (fail . snd) return =<< f g

runGlobalWai :: Global -> M Wai.Response -> IO Wai.Response
runGlobalWai g (ExceptT (ReaderT f)) = either (\(s, m) -> response s [] m) id <$> f g

runErr :: Err a -> M a
runErr = liftEither . runExcept

type Action = Wai.Request -> M Wai.Response
type Route a = R.RouteAction a Action

getPath :: R.Path p -> (p -> Action) -> R.RouteAction p Action
getPath p = R.RouteAction $ R.routeMethod R.GET R.*< R.routePath p

askCatalog :: Simulation -> M Catalog
askCatalog sim = maybe
  (raise404 "No such simulation")
  return =<< asks (HM.lookup sim . catalogMap . globalCatalogs)

once :: M a -> M (IO a)
once act = do
  g <- ask
  liftIO $ do
    mv <- newMVar Nothing
    return $ modifyMVar mv $
      fmap (\v -> (Just v, v))
        . maybe (runGlobal g act) return
