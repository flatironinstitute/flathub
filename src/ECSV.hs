{-# LANGUAGE OverloadedStrings #-}

module ECSV
  ( 
  ) where

import           Control.Arrow (first)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Yaml as Y

data ECSVColumn = ECSVColumn
  { ecsvColName :: String
  , ecsvColDatatype :: String -- bool, int8, int16, int32, int64, uint8, uint16, uint32, uint64, float16, float32, float64, float128, complex64, complex128, complex256, and string
  , ecsvColSubtype :: Maybe String
  , ecsvColUnit :: Maybe String
  , ecsvColFormat :: Maybe String
  , ecsvColDescription :: Maybe String
  , ecsvColMeta :: J.Value
  } deriving (Show)

data ECSVHeader = ECSVHeader
  { ecsvDelimiter :: Char
  , ecsvDatatype :: [ECSVColumn]
  , ecsvMeta :: J.Value
  , ecsvSchema :: Maybe String
  } deriving (Show)

instance J.FromJSON ECSVColumn where
  parseJSON = J.withObject "ecsv column header" $ \o -> ECSVColumn
    <$> o J..: "name"
    <*> o J..: "datatype"
    <*> o J..:? "subtype"
    <*> o J..:? "unit"
    <*> o J..:? "format"
    <*> o J..:? "description"
    <*> o J..:? "meta" J..!= J.Null

instance J.FromJSON ECSVHeader where
  parseJSON = J.withObject "ecsv header" $ \o -> ECSVHeader
    <$> o J..:? "delimiter" J..!= ' '
    <*> o J..: "datatype"
    <*> o J..:? "meta" J..!= J.Null
    <*> o J..:? "schema"

splitLine :: BSL.ByteString -> (BSL.ByteString, BSL.ByteString)
splitLine l = maybe (l, BSL.empty) (\i -> BSL.splitAt (succ i) l) $ BSLC.elemIndex '\n' l

splitHeader :: BSL.ByteString -> (BSL.ByteString, BSL.ByteString)
splitHeader f = case BSL.splitAt 2 f of
  ("##", r) -> splitHeader $ snd $ splitLine r
  ("# ", lr) -> 
    let (l, r) = splitLine lr in
    first (l <>) $ splitHeader r
  _ -> (BSL.empty, f)

parseHeader :: BSL.ByteString -> (Either Y.ParseException ECSVHeader, BSL.ByteString)
parseHeader = first 
  (maybe (Left $ Y.InvalidYaml $ Just $ Y.YamlException "missing %ECSV vesrion header")
    (Y.decodeEither' . BSL.toStrict)
    . BSL.stripPrefix "%ECSV 1.0\n")
  . splitHeader
