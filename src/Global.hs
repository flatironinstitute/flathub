{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Global
  ( Global(..)
  , M
  , runGlobal
  , Action
  , Route
  , Simulation
  , getPath
  , askCatalog
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import           Control.Monad.Reader (ReaderT, runReaderT, asks)
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Status (notFound404)
import qualified Network.Wai as Wai
import qualified Waimwork.Config as C
import           Waimwork.Response (response)
import           Waimwork.Result (result)
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

type M = ReaderT Global IO

runGlobal :: Global -> M a -> IO a
runGlobal = flip runReaderT

type Action = Wai.Request -> M Wai.Response
type Route a = R.RouteAction a Action

getPath :: R.Path p -> (p -> Action) -> R.RouteAction p Action
getPath p = R.RouteAction $ R.routeMethod R.GET R.*< R.routePath p

askCatalog :: Simulation -> M Catalog
askCatalog sim = maybe
  (result $ response notFound404 [] ("No such simulation" :: BSC.ByteString))
  return =<< asks (HM.lookup sim . catalogMap . globalCatalogs)
