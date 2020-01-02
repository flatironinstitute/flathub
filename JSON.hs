module JSON
  ( parseJSONField
  , (.=*)
  , EmptyJSON(..)
  , mergeJSON, mergeJSONObject
  , unsingletonJSON
  , jsLazyByteStringValue
  ) where

import qualified Data.Aeson.Types as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import           Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word8)
import qualified Text.Blaze.Html5 as H
import qualified Waimwork.Blaze as WH

parseJSONField :: T.Text -> (J.Value -> J.Parser a) -> J.Object -> J.Parser a
parseJSONField f p o = J.explicitParseField p o f

infixr 8 .=*
(.=*) :: T.Text -> J.Series -> J.Series
(.=*) f = JE.pair f . JE.pairs

data EmptyJSON = EmptyJSON

instance J.ToJSON EmptyJSON where
  toJSON EmptyJSON = J.emptyObject
  toEncoding EmptyJSON = JE.emptyObject_

instance J.FromJSON EmptyJSON where
  parseJSON J.Null = return EmptyJSON
  parseJSON (J.Object o) | HM.null o = return EmptyJSON
  parseJSON (J.Array o) | V.null o = return EmptyJSON
  parseJSON v = J.typeMismatch "EmptyJSON" v

mergeJSON :: J.Value -> J.Value -> J.Value
mergeJSON J.Null x = x
mergeJSON (J.Object a) (J.Object b) = J.Object (mergeJSONObject a b)
mergeJSON x _ = x

mergeJSONObject :: J.Object -> J.Object -> J.Object
mergeJSONObject = HM.unionWith mergeJSON

unsingletonJSON :: J.Value -> J.Value
unsingletonJSON (J.Array v) | V.length v == 1 = V.head v
unsingletonJSON v = v

wordEscaped :: Char -> BP.BoundedPrim Word8
wordEscaped q =
  BP.condB (== c2w q) (backslash q) $
  BP.condB (== c2w '\\') (backslash '\\') $
  BP.condB (>= c2w ' ') (BP.liftFixedToBounded BP.word8) $
  BP.condB (== c2w '\n') (backslash 'n') $
  BP.condB (== c2w '\r') (backslash 'r') $
  BP.condB (== c2w '\t') (backslash 't') $
    BP.liftFixedToBounded $ (\c -> ('\\', ('u', fromIntegral c))) BP.>$< BP.char8 BP.>*< BP.char8 BP.>*< BP.word16HexFixed
  where
  backslash c = BP.liftFixedToBounded $ const ('\\', c) BP.>$< BP.char8 BP.>*< BP.char8

-- | Escape (but do not quote) a LazyByteString
escapeJSLazyByteString :: Char -> BSL.ByteString -> B.Builder
escapeJSLazyByteString = BP.primMapLazyByteStringBounded . wordEscaped

jsLazyByteStringValue :: BSL.ByteString -> H.AttributeValue
jsLazyByteStringValue s = H.toValue q <> WH.builderValue (escapeJSLazyByteString q s) <> H.toValue q
  where q = '\''
