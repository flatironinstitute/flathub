{-# LANGUAGE OverloadedStrings #-}

-- |https://github.com/astropy/astropy-APEs/blob/master/APE6.rst
module Output.ECSV
  ( ecsvHeader
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T

import Monoid
import JSON ((.=*))
import Catalog
import Field
import Output.CSV

ecsvType :: Type -> T.Text
ecsvType (Text _)      = "string"
ecsvType (Keyword _)   = "string"
ecsvType (Long _)      = "int64"
ecsvType (Integer _)   = "int32"
ecsvType (Short _)     = "int16"
ecsvType (Byte _)      = "int8"
ecsvType (Double _)    = "float64"
ecsvType (Float _)     = "float32"
ecsvType (HalfFloat _) = "float16"
ecsvType (Boolean _)   = "bool"
ecsvType (Void _)      = "string"

ecsvHeader :: Catalog -> Query -> B.Builder
ecsvHeader cat query = (mintersperseMap (B.char8 '\n') (B.string8 "# " <>) $
  [ B.string8 "%ECSV 0.9"
  , B.string8 "---"
  , B.string8 "delimiter: ','"
  , B.string8 "meta:"
  , kv "  name" $ catalogTitle cat
  , kv "  description" $ catalogDescr cat
  , kv "  query" query
  , B.string8 "datatype:"
  ] ++ map (\f -> B.string8 "- " <> J.fromEncoding (JE.pairs $
       "name" J..= fieldName f
    <> "datatype" J..= ecsvType (fieldType f)
    <> "unit" J..= fieldUnits f
    <> "description" J..= (fieldTitle f <> foldMap (": " <>) (fieldDescr f))
    <> foldMap (\e -> "meta" .=* ("enum" J..= e)) (fieldEnum f)
    )
  ) (queryFields query))
  <> B.char8 '\n' <> csvTextRow (map fieldName $ queryFields query)
  where
  kv k v = B.string8 k <> ": " <> J.fromEncoding (J.toEncoding v)
