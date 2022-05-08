{-# LANGUAGE OverloadedStrings #-}

-- |https://docs.astropy.org/en/stable/io/ascii/ecsv.html
module Output.ECSV
  ( ecsvHeader
  , ecsvField
  ) where

import qualified Data.Aeson as J
import qualified Data.ByteString.Builder as B
import           Data.Maybe (maybeToList)

import Catalog
import Type
import Field
import Data.ECSV

ecsvType :: Type -> ECSVDataType
ecsvType (Keyword _)   = ECSVString
ecsvType (Long _)      = ECSVInt64
ecsvType (ULong _)     = ECSVUInt64
ecsvType (Integer _)   = ECSVInt32
ecsvType (Short _)     = ECSVInt16
ecsvType (Byte _)      = ECSVInt8
ecsvType (Double _)    = ECSVFloat64
ecsvType (Float _)     = ECSVFloat32
ecsvType (HalfFloat _) = ECSVFloat16
ecsvType (Boolean _)   = ECSVBool
ecsvType (Array _)     = ECSVString
ecsvType (Void _)      = error "ecsvType Void"

ecsvField :: Field -> ECSVColumn
ecsvField f = ECSVColumn
  { ecsvColName = fieldName f
  , ecsvColDataType = ecsvType (fieldType f)
  , ecsvColSubtype = (`ECSVSubTypeArray` [Nothing]) . ecsvType <$> typeIsArray (fieldType f)
  , ecsvColFormat = Nothing
  , ecsvColUnit = fieldUnits f
  , ecsvColDescription = Just $ fieldTitle f <> foldMap (": " <>) (fieldDescr f)
  , ecsvColMeta = Just $ J.object $
    maybeToList (("enum" J..=) <$> fieldEnum f)
  }

ecsvHeader :: Catalog -> Query -> B.Builder
ecsvHeader cat query = renderECSVHeader $ ECSVHeader
  { ecsvDelimiter = ','
  , ecsvDatatype = map ecsvField (queryFields query)
  , ecsvMeta = Just $ J.object
    [ "name" J..= catalogTitle cat
    , "description" J..= catalogDescr cat
    , "query" J..= query
    ]
  , ecsvSchema = Nothing
  }
