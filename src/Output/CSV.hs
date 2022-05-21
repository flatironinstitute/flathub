module Output.CSV
  ( csvTextRow
  , csvJSONRow
  , csvOutput
  , csvOutput'
  ) where

import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Functor.Identity (Identity(Identity), runIdentity)
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.Word (Word8)

import Monoid
import Type
import Field
import Catalog
import Backend
import Global
import Output.Types

dQuote :: Word8
dQuote = c2w '"'

bQuote :: B.Builder
bQuote = B.word8 dQuote

csvQuote :: BP.BoundedPrim Word8
csvQuote = BP.condB (== dQuote)
  (BP.liftFixedToBounded $ const (dQuote, dQuote) BP.>$< BP.word8 BP.>*< BP.word8)
  (BP.liftFixedToBounded BP.word8) where

_csvByteString :: BS.ByteString -> B.Builder
_csvByteString t
  | BSC.any (`elem` "\",\r\n") t = bQuote <> BP.primMapByteStringBounded csvQuote t <> bQuote
  | otherwise = B.byteString t

csvLazyByteString :: BSL.ByteString -> B.Builder
csvLazyByteString t
  | BSLC.any (`elem` "\",\r\n") t = bQuote <> BP.primMapLazyByteStringBounded csvQuote t <> bQuote
  | otherwise = B.lazyByteString t

csvText :: T.Text -> B.Builder
csvText t
  | T.any (`elem` "\",\r\n") t = bQuote <> TE.encodeUtf8BuilderEscaped csvQuote t <> bQuote
  | otherwise = TE.encodeUtf8Builder t

csvJSON :: J.Value -> B.Builder
csvJSON J.Null = mempty
csvJSON (J.String t) = csvText t
csvJSON x@(J.Number _) = J.fromEncoding $ J.toEncoding x
csvJSON x@(J.Bool _) = J.fromEncoding $ J.toEncoding x
csvJSON x = csvLazyByteString $ J.encode x

csvValue :: Value -> B.Builder
csvValue (Keyword (Identity t)) = csvText t
csvValue (Array v) = csvLazyByteString $ J.encode v
csvValue v = unTypeValue (renderValue . runIdentity) v

csvMaybeValue :: TypeValue Maybe -> B.Builder
csvMaybeValue = foldMap csvValue . sequenceValue

csvBuilderMap :: (a -> B.Builder) -> [a] -> B.Builder
csvBuilderMap f r = mintersperseMap (B.char8 ',') f r <> B.char8 '\n'

csvTextRow :: [T.Text] -> B.Builder
csvTextRow = csvBuilderMap csvText

csvJSONRow :: [J.Value] -> B.Builder
csvJSONRow = csvBuilderMap csvJSON

_csvValueRow :: [Value] -> B.Builder
_csvValueRow = csvBuilderMap csvValue

csvMaybeValueRow :: [TypeValue Maybe] -> B.Builder
csvMaybeValueRow = csvBuilderMap csvMaybeValue

csvGenerator :: Catalog -> DataArgs V.Vector -> M OutputStream
csvGenerator cat args = outputStreamRows Nothing
  (csvTextRow $ map fieldName $ V.toList $ dataFields args)
  (csvMaybeValueRow . V.toList)
  mempty
  cat args

csvOutput :: OutputFormat
csvOutput = OutputFormat
  { outputMimeType = fromString "text/csv"
  , outputExtension = "csv"
  , outputDescription = T.pack "Standard CSV file with a header of field names; missing values are represented by empty fields, arrays are encoded as JSON"
  , outputGenerator = \_ -> csvGenerator
  }

csvGenerator' :: Catalog -> DataArgs V.Vector -> M OutputStream
csvGenerator' cat args = outputStreamRows' Nothing
  (csvTextRow $ map fieldName $ V.toList $ dataFields args)
  (csvMaybeValueRow . V.toList)
  mempty
  cat args

csvOutput' :: OutputFormat
csvOutput' = OutputFormat
  { outputMimeType = fromString "text/csv"
  , outputExtension = "csvs"
  , outputDescription = T.pack "Standard CSV file with a header of field names; missing values are represented by empty fields, arrays are encoded as JSON"
  , outputGenerator = \_ -> csvGenerator'
  }
