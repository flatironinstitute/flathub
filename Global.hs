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
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Control.Monad.Reader (ReaderT, runReaderT)
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wai as Wai
import qualified Waimwork.Config as C
import qualified Waimwork.Database.PostgreSQL as PG
import qualified Web.Route.Invertible as R

import Schema

data Global = Global
  { globalConfig :: C.Config
  , globalHTTP :: HTTP.Manager
  , globalES :: HTTP.Request
  , globalPG :: PG.DBPool
  , globalCatalogs :: HM.HashMap Simulation Catalog
  }

type M = ReaderT Global IO

runGlobal :: Global -> M a -> IO a
runGlobal = flip runReaderT

type Action = Wai.Request -> M Wai.Response
type Route a = R.RouteAction a Action

type Simulation = T.Text
