module CSV
  ( csvTextRow
  , csvJSONRow
  ) where

import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word (Word8)

inter :: B.Builder -> [B.Builder] -> B.Builder
inter _ [] = mempty
inter d (x:l) = x <> mconcat (map (d <>) l)

dQuote :: Word8
dQuote = c2w '"'

bQuote :: B.Builder
bQuote = B.word8 dQuote

csvQuote :: BP.BoundedPrim Word8
csvQuote = BP.condB (== dQuote)
  (BP.liftFixedToBounded $ const (dQuote, dQuote) BP.>$< BP.word8 BP.>*< BP.word8)
  (BP.liftFixedToBounded BP.word8) where

csvByteString :: BS.ByteString -> B.Builder
csvByteString t
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

csvBuilderRow :: [B.Builder] -> B.Builder
csvBuilderRow r = inter (B.char8 ',') r <> B.char8 '\n'

csvTextRow :: [T.Text] -> B.Builder
csvTextRow = csvBuilderRow . map csvText

csvJSONRow :: [J.Value] -> B.Builder
csvJSONRow = csvBuilderRow . map csvJSON
