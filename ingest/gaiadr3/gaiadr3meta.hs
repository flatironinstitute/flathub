-- Export gaia metadata from YAML ECSV headers to generate initial catalog template
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (maybeToList)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Yaml as Y
import           System.Environment (getArgs)

import Data.ECSV
import Type

dataType :: ECSVDataType -> Type
dataType ECSVString  = Keyword Proxy
dataType ECSVInt64   = Long Proxy
dataType ECSVUInt64  = ULong Proxy
dataType ECSVInt32   = Integer Proxy
dataType ECSVInt16   = Short Proxy
dataType ECSVInt8    = Byte Proxy
dataType ECSVFloat64 = Double Proxy
dataType ECSVFloat32 = Float Proxy
dataType ECSVFloat16 = HalfFloat Proxy
dataType ECSVBool    = Boolean Proxy
dataType t           = error $ "unhandled type: " ++ show t

columnType :: ECSVColumn -> Type
columnType ECSVColumn{ ecsvColDataType = ECSVString, ecsvColSubtype = Just (ECSVSubTypeArray t [Nothing]) } = Array $ singletonArray $ dataType t
columnType ECSVColumn{ ecsvColDataType = t, ecsvColSubtype = Nothing } = dataType t
columnType c = error $ "unhandled type: " ++ show c

columnField :: ECSVColumn -> J.Value
columnField c@ECSVColumn{..} = J.object $
  [ "name" J..= ecsvColName
  , "type" J..= t
  ] ++ concatMap maybeToList
  [ ("units" J..=) <$> ecsvColUnit
  , ("descr" J..=) <$> ecsvColDescription
  , ("store" J..= True) <$ typeIsArray t
  ] where t = columnType c

tableGroup :: ECSVHeader -> J.Value
tableGroup ECSVHeader{..} = J.object
  [ "name" J..= meta HM.! "TABLE"
  , "descr" J..= meta HM.! "DESCRIPTION"
  , "sub" J..= fmap columnField ecsvDatatype
  ] where
  meta :: HM.HashMap T.Text T.Text
  meta = either error mconcat $ maybe (Left "missing meta") (J.parseEither J.parseJSON) ecsvMeta

main = do
  args <- mapM Y.decodeFileThrow =<< getArgs
  BSC.putStr $ Y.encode $ map tableGroup args
