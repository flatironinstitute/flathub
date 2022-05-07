{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Global
  ( Global(..)
  , M
  , runGlobal
  , runGlobalWai
  , E
  , runE
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
import           Control.Monad.Except (MonadError, ExceptT(..), throwError, liftEither, runExcept)
import           Control.Monad.Reader (ReaderT(..), asks)
import           Data.Functor.Identity (Identity)
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Status (Status, badRequest400, notFound404)
import qualified Network.Wai as Wai
import qualified Waimwork.Config as C
import           Waimwork.Response (response)
import qualified Web.Route.Invertible as R

import Catalog

data Global = Global
  { globalConfig :: C.Config
  , globalHTTP :: HTTP.Manager
  , globalES :: HTTP.Request
  , globalCatalogs :: Catalogs
  , globalDataDir :: FilePath
  , globalDevMode :: Bool
  }

type ErrT = ExceptT (Status, String)
type E = ErrT Identity
type M = ErrT (ReaderT Global IO)

runGlobal :: Global -> M a -> IO a
runGlobal g (ExceptT (ReaderT f)) = either (fail . snd) return =<< f g

runGlobalWai :: Global -> M Wai.Response -> IO Wai.Response
runGlobalWai g (ExceptT (ReaderT f)) = either (\(s, m) -> response s [] m) id <$> f g

runE :: E a -> M a
runE = liftEither . runExcept

raise :: MonadError (Status, String) m => Status -> String -> m a
raise = curry throwError 

raise400, raise404 :: MonadError (Status, String) m => String -> m a
raise400 = raise badRequest400
raise404 = raise notFound404

type Action = Wai.Request -> M Wai.Response
type Route a = R.RouteAction a Action

getPath :: R.Path p -> (p -> Action) -> R.RouteAction p Action
getPath p = R.RouteAction $ R.routeMethod R.GET R.*< R.routePath p

askCatalog :: Simulation -> M Catalog
askCatalog sim = maybe
  (raise404 "No such simulation")
  return =<< asks (HM.lookup sim . catalogMap . globalCatalogs)

once :: M a -> M (IO a)
once act = ExceptT $ ReaderT $ \g -> do
  mv <- newMVar Nothing
  return $ Right $ modifyMVar mv $
    fmap (\v -> (Just v, v))
      . maybe (runGlobal g act) return
