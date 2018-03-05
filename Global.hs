{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Global where

import           Data.Array (Array, Ix)
import qualified Data.Text as T
import           Control.Monad.Reader (ReaderT, runReaderT)
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wai as Wai
import           Text.Read (readPrec, Lexeme(Ident), lexP, readMaybe)
import qualified Waimwork.Config as C
import qualified Web.Route.Invertible as R

import qualified ES.Types as ES

data Global = Global
  { globalConfig :: C.Config
  , globalHTTP :: HTTP.Manager
  , globalES :: ES.Server
  , globalIndices :: Array Simulation ES.Index
  }

type M = ReaderT Global IO

runGlobal :: Global -> M a -> IO a
runGlobal = flip runReaderT

type Action = Wai.Request -> M Wai.Response
type Route a = R.RouteAction a Action

data Simulation
  = IllustrisGroup
  -- | IllustrisSubGroup
  | Neutrino
  deriving (Eq, Enum, Bounded, Ord, Ix)

instance Show Simulation where
  show IllustrisGroup    = "illustris"
  -- show IllustrisSubGroup = "illustris_sub"
  show Neutrino          = "neutrino"

instance Read Simulation where
  readPrec = do
    Ident s <- lexP
    case s of
      "illustris"      -> return IllustrisGroup
      -- "illustris_sub"  -> return IllustrisSubGroup
      "neutrino"       -> return Neutrino
      _ -> fail "Unknown simulation"

instance R.Parameter R.PathString Simulation where
  parseParameter = readMaybe . T.unpack
  renderParameter = T.pack . show
