{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.ECSV
  ( ECSVDataType(..)
  , ECSVSubType(..)
  , ECSVColumn(..)
  , ECSVHeader(..)
  , parseECSVHeader
  , renderECSVHeader
  ) where

import           Control.Arrow (first)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import           Text.Read (readEither)

data ECSVDataType
  = ECSVBool
  | ECSVInt8
  | ECSVInt16
  | ECSVInt32
  | ECSVInt64
  | ECSVUInt8
  | ECSVUInt16
  | ECSVUInt32
  | ECSVUInt64
  | ECSVFloat16
  | ECSVFloat32
  | ECSVFloat64
  | ECSVFloat128
  | ECSVComplex64
  | ECSVComplex128
  | ECSVComplex256
  | ECSVString

$(J.deriveJSON J.defaultOptions{ J.constructorTagModifier = map toLower . drop 4 } ''ECSVDataType)

data ECSVSubType
  = ECSVSubTypeArray ECSVDataType [Maybe Word]
  | ECSVSubTypeJSON
  | ECSVSubTypeOther T.Text

instance J.ToJSON ECSVSubType where
  toJSON (ECSVSubTypeArray t d) =
    J.String $ s <> ('[' `T.cons` T.intercalate "," (map (maybe "null" (T.pack . show)) d) `T.snoc` ']')
    where J.String s = J.toJSON t
  toJSON ECSVSubTypeJSON = J.String "json"
  toJSON (ECSVSubTypeOther s) = J.String s

instance J.FromJSON ECSVSubType where
  parseJSON (J.String "json") = return ECSVSubTypeJSON
  parseJSON (J.String s) 
    | Just d <- T.stripSuffix "]" =<< T.stripPrefix "[" ds = do
      t <- J.parseJSON (J.String ts)
      dl <- mapM pd $ T.split (',' ==) d
      return $ ECSVSubTypeArray t dl
    | otherwise = return $ ECSVSubTypeOther s
    where
    pd "null" = return Nothing
    pd d = either fail return $ readEither $ T.unpack d
    (ts, ds) = T.break ('[' ==) s
  parseJSON j = J.typeMismatch "ECSVSubType" j

data ECSVColumn = ECSVColumn
  { ecsvColName :: T.Text
  , ecsvColDataType :: ECSVDataType
  , ecsvColSubtype :: Maybe ECSVSubType
  , ecsvColUnit :: Maybe T.Text
  , ecsvColFormat :: Maybe T.Text
  , ecsvColDescription :: Maybe T.Text
  , ecsvColMeta :: Maybe J.Value
  }

$(J.deriveJSON J.defaultOptions{ J.fieldLabelModifier = map toLower . drop 7, J.omitNothingFields = True } ''ECSVColumn)

data ECSVHeader = ECSVHeader
  { ecsvDelimiter :: Char -- optional, should only be ' ' or ','
  , ecsvDatatype :: [ECSVColumn]
  , ecsvMeta :: Maybe J.Value
  , ecsvSchema :: Maybe T.Text
  }

$(J.deriveToJSON J.defaultOptions{ J.fieldLabelModifier = map toLower . drop 4, J.omitNothingFields = True } ''ECSVHeader)

instance J.FromJSON ECSVHeader where
  parseJSON = J.withObject "ECSVHeader" $ \o -> ECSVHeader
    <$> o J..:? "delimiter" J..!= ' ' -- need instance for this default
    <*> o J..: "datatype"
    <*> o J..:? "meta"
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

-- |Parse the header of an ECSV file, returning the remaining CSV portion
parseECSVHeader :: BSL.ByteString -> (Either Y.ParseException ECSVHeader, BSL.ByteString)
parseECSVHeader = first
  (maybe (Left $ Y.InvalidYaml $ Just $ Y.YamlException "missing %ECSV version header")
    (Y.decodeEither' . BSL.toStrict)
    . BSL.stripPrefix "%ECSV 1.0\n")
  . splitHeader

-- |Render the header of an ECSV file, including all the comment and YAML lines, but not including the CSV header line
renderECSVHeader :: ECSVHeader -> B.Builder
renderECSVHeader h =
  foldMap ((<> B.char7 '\n') . (B.string7 "# " <>) . B.byteString)
  $ [ "%ECSV 1.0"
    , "---"
    ] ++ BSC.lines (Y.encode h)

{-
type ECSVScalar = ECSVDataType' Identity
type ECSVList = ECSVDataType' []
data ECSVValue = ECSVScalar ECSVScalar
               | ECSVList ECSVList
               | ESCVNothing
  deriving ( Show )

deriving instance Show ECSVDataType
deriving instance Show ECSVScalar
deriving instance Show ECSVList


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
-}
