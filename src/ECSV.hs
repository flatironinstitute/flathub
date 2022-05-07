{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           Control.Arrow (first)
import           Control.Monad (when, zipWithM_)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Yaml as Y
import qualified Data.Csv.Streaming as CSV
import Data.Foldable ( for_ )
import Data.Functor.Identity ( Identity ( Identity ) )
import Data.Proxy ( Proxy ( Proxy ) )
import Data.Int ( Int8, Int16, Int32, Int64 )
import System.Environment (getArgs)


data ECSVDataType' f
  = ECSVFloat32   !(f Float)
  | ECSVFloat64   !(f Double)
  | ECSVInt8      !(f Int8)
  | ECSVInt16     !(f Int16)
  | ECSVInt32     !(f Int32)
  | ECSVInt64     !(f Int64)
  | ECSVBool      !(f Bool)
  | ECSVString    !(f String)
  

type ECSVDataType = ECSVDataType' Proxy
type ECSVScalar = ECSVDataType' Identity
type ECSVList = ECSVDataType' []
data ECSVValue = ECSVScalar ECSVScalar
               | ECSVList ECSVList
               | ESCVNothing
  deriving ( Show )

deriving instance Show ECSVDataType
deriving instance Show ECSVScalar
deriving instance Show ECSVList

data ECSVSubType
  = ECSVSubTypeNone
  | ECSVSubTypeArray ECSVDataType
  deriving (Show)

data ECSVColumn = ECSVColumn
  { ecsvColName :: String
  , ecsvColDatatype :: ECSVDataType
  , ecsvColSubtype :: ECSVSubType
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


instance J.FromJSON ECSVDataType where
  parseJSON = J.withText "ecsv data type" $ \o -> case o of 
    "bool" -> return $ ECSVBool Proxy
    "int8" -> return $ ECSVInt8 Proxy
    "int16" -> return $ ECSVInt16 Proxy
    "int32" -> return $ ECSVInt32 Proxy
    "int64" -> return $ ECSVInt64 Proxy
    "float32" -> return $ ECSVFloat32 Proxy
    "float64" -> return $ ECSVFloat64 Proxy
    "string" -> return $ ECSVString Proxy
    _ -> fail (show o)

-- FIXME: hardcoded variable length arrays. not all types.
instance J.FromJSON ECSVSubType where
  parseJSON = J.withText "ecsv subtype" $ \o -> case o of
    "bool[null]" -> return (ECSVSubTypeArray $ ECSVBool Proxy)
    "int8[null]" -> return (ECSVSubTypeArray $ ECSVInt8 Proxy)
    "int32[null]" -> return (ECSVSubTypeArray $ ECSVInt32 Proxy)
    "int64[null]" -> return (ECSVSubTypeArray $ ECSVInt64 Proxy)
    "float32[null]" -> return (ECSVSubTypeArray $ ECSVFloat32 Proxy)
    "float64[null]" -> return (ECSVSubTypeArray $ ECSVFloat64 Proxy)
    _ -> fail (show o)

instance J.FromJSON ECSVColumn where
  parseJSON = J.withObject "ecsv column header" $ \o -> ECSVColumn
    <$> o J..: "name"
    <*> o J..: "datatype"
    <*> o J..:? "subtype" J..!= ECSVSubTypeNone
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
  (maybe (Left $ Y.InvalidYaml $ Just $ Y.YamlException "missing %ECSV version header")
    (Y.decodeEither' . BSL.toStrict)
    . BSL.stripPrefix "%ECSV 1.0\n")
  . splitHeader


processRow :: [ECSVColumn] -> [String] -> [ECSVValue]
processRow =
  zipWith fx
  where
    fx (ECSVColumn { ecsvColDatatype = ECSVBool Proxy }) "True" = ECSVScalar $ ECSVBool $ Identity True
    fx (ECSVColumn { ecsvColDatatype = ECSVBool Proxy }) "False" = ECSVScalar $ ECSVBool $ Identity False
    fx (ECSVColumn { ecsvColDatatype = ECSVInt16 Proxy }) s = ECSVScalar $ ECSVInt16 $ Identity (read s)
    fx (ECSVColumn { ecsvColDatatype = ECSVInt64 Proxy }) s = ECSVScalar $ ECSVInt64 $ Identity (read s)
    fx (ECSVColumn { ecsvColDatatype = ECSVString Proxy, ecsvColSubtype = ECSVSubTypeArray (ECSVBool Proxy) }) s = ECSVList $ ECSVBool $ read s
    fx (ECSVColumn { ecsvColDatatype = ECSVString Proxy, ecsvColSubtype = ECSVSubTypeArray (ECSVFloat32 Proxy) }) s = ECSVList $ ECSVFloat32 $ read s
    fx (ECSVColumn { ecsvColDatatype = ECSVString Proxy, ecsvColSubtype = ECSVSubTypeArray (ECSVFloat64 Proxy) }) s = ECSVList $ ECSVFloat64 $ read s
    fx (ECSVColumn { ecsvColDatatype = ECSVString Proxy, ecsvColSubtype = ECSVSubTypeArray (ECSVInt8 Proxy) }) s = ECSVList $ ECSVInt8 $ read s
    fx (ECSVColumn { ecsvColDatatype = ECSVString Proxy, ecsvColSubtype = ECSVSubTypeArray (ECSVInt64 Proxy) }) s = ECSVList $ ECSVInt64 $ read s


main :: IO ()
main = do
  [filePath] <- getArgs
  l <- BSLC.readFile filePath
  let (headerOrErr, rest) = parseHeader l
  -- header <- case headerOrErr of
  --   Left pe -> (fail . show) pe
  --   Right eh -> return eh
  metaHeader <- either (fail . show) return headerOrErr
  let records = CSV.decode CSV.NoHeader rest
  (csvHeader, rows) :: ([String], CSV.Records [String]) <- case records of
    CSV.Cons e re -> ( , re) <$> either fail return e
    CSV.Nil m_s bs -> fail (maybe "No data" id m_s)

  when ( (map ecsvColName $ ecsvDatatype metaHeader) /= csvHeader ) $ fail "Headers don't match"
  print metaHeader
  mapM_ print $ fmap (processRow (ecsvDatatype metaHeader)) rows


  -- print $ map ecsvColName $ ecsvDatatype metaHeader
  -- print csvHeader
