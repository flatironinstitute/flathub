{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Global where

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import           Data.Hashable (Hashable, hashWithSalt)
import qualified Data.HashMap.Strict as HM
import           Data.String (IsString)
import qualified Data.Text as T
import           Control.Monad.Reader (ReaderT, runReaderT)
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wai as Wai
import           Text.Read (readPrec, Lexeme(Ident), lexP, readMaybe)
import qualified Waimwork.Config as C
import qualified Web.Route.Invertible as R

import qualified ES.Types as ES
import Schema

data Global = Global
  { globalConfig :: C.Config
  , globalHTTP :: HTTP.Manager
  , globalES :: ES.Server
  , globalCatalogs :: HM.HashMap Simulation Catalog
  }

type M = ReaderT Global IO

runGlobal :: Global -> M a -> IO a
runGlobal = flip runReaderT

type Action = Wai.Request -> M Wai.Response
type Route a = R.RouteAction a Action

data Simulation
  = Illustris
  | IllustrisSub
  | Neutrino
  | GAEA
  deriving (Eq, Enum, Bounded, Ord)

instance Hashable Simulation where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Show Simulation where
  show Illustris    = "illustris"
  show IllustrisSub = "illustris_sub"
  show Neutrino     = "neutrino"
  show GAEA         = "gaea"

parseSimulation :: (Monad m, IsString s, Eq s) => s -> m Simulation
parseSimulation "illustris"      = return Illustris
parseSimulation "illustris_sub"  = return IllustrisSub
parseSimulation "neutrino"       = return Neutrino
parseSimulation "gaea"           = return GAEA
parseSimulation _ = fail "Unknown simulation"

instance Read Simulation where
  readPrec = do
    Ident s <- lexP
    parseSimulation s

instance R.Parameter R.PathString Simulation where
  parseParameter = readMaybe . T.unpack
  renderParameter = T.pack . show

instance J.ToJSON Simulation where
  toJSON = J.String . T.pack . show
instance J.ToJSONKey Simulation where
  toJSONKey = J.ToJSONKeyText t (JE.text . t) where
    t = T.pack . show

instance J.FromJSON Simulation where
  parseJSON = J.withText "simulation" parseSimulation
instance J.FromJSONKey Simulation where
  fromJSONKey = J.FromJSONKeyTextParser parseSimulation
