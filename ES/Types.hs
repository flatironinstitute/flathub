{-# LANGUAGE OverloadedStrings #-}

module ES.Types
  ( Server(..)
  , Type(..)
  ) where

import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import           Data.Default (Default(def))
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import           Text.Read (readPrec, Lexeme(Ident), lexP, readEither)

data Server = Server
  { serverRequest :: !HTTP.Request
  }

data Type
  = Text
  | Keyword
  | Long
  | Integer
  | Short
  | Byte
  | Double
  | Float
  | HalfFloat
  | ScaledFloat Double
  | Date
  | Boolean
  | Binary
  deriving (Eq)

instance Default Type where
  def = Float

instance Show Type where
  show Text             = "text"
  show Keyword          = "keyword"
  show Long             = "long"
  show Integer          = "integer"
  show Short            = "short"
  show Byte             = "byte"
  show Double           = "double"
  show Float            = "float"
  show HalfFloat        = "half_float"
  show (ScaledFloat _)  = "scaled_float"
  show Date             = "date"
  show Boolean          = "boolean"
  show Binary           = "binary"

instance Read Type where
  readPrec = do
    Ident s <- lexP
    case s of
      "text"        -> return Text
      "keyword"     -> return Keyword
      "long"        -> return Long
      "integer"     -> return Integer
      "short"       -> return Short
      "byte"        -> return Byte
      "double"      -> return Double
      "float"       -> return Float
      "half_float"  -> return HalfFloat
      "date"        -> return Date
      "boolean"     -> return Boolean
      "binary"      -> return Binary
      _ -> fail "Unknown ES type"
      
instance J.ToJSON Type where
  toJSON = J.toJSON . show
  toEncoding = J.toEncoding . show

instance J.FromJSON Type where
  parseJSON = J.withText "ES type" $ either fail return . readEither . T.unpack
