{-# LANGUAGE OverloadedStrings #-}

module Output.Numpy
  ( numpyHeader
  , numpyRow
  , numpyOutput
  ) where

import qualified Data.Aeson.Types as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (fold)
import           Data.Functor.Compose (getCompose)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (stimesMonoid)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.Word (Word16, Word32, Word64)
import           Foreign.C.Types (CUShort(CUShort))
import           Numeric.Half (Half(Half))
import           Unsafe.Coerce (unsafeCoerce)

import Type
import Field
import Catalog
import Monoid
import Output.Types
import Global
import Backend

getHalf' :: Half -> Word16
getHalf' (Half (CUShort x)) = x

numpyString :: Word -> Maybe T.Text -> B.Builder
numpyString l t = B.byteString s <> stimesMonoid (l' - BS.length s) (B.char7 '\0') where
  s = BS.take l' $ foldMap TE.encodeUtf8 t
  l' = fromIntegral l

numpyBuild :: Field -> TypeValue Maybe -> B.Builder
numpyBuild _ (Double    x) = B.doubleLE $ fromMaybe (unsafeCoerce (0x7ff80000ffffffff::Word64)) x
numpyBuild _ (Float     x) = B.floatLE  $ fromMaybe (unsafeCoerce (0x7fc0ffff::Word32)) x
numpyBuild _ (HalfFloat x) = B.word16LE $ maybe 0x7cff getHalf' x
numpyBuild _ (Long      x) = B.int64LE  $ fromMaybe minBound x
numpyBuild _ (ULong     x) = B.word64LE $ fromMaybe maxBound x
numpyBuild _ (Integer   x) = B.int32LE  $ fromMaybe minBound x
numpyBuild _ (Short     x) = B.int16LE  $ fromMaybe minBound x
numpyBuild _ (Byte      x) = B.int8     $ fromMaybe minBound x
numpyBuild _ (Boolean Nothing) = B.int8 0
numpyBuild _ (Boolean (Just False)) = B.int8 0
numpyBuild _ (Boolean (Just True)) = B.int8 1
numpyBuild f (Keyword   x) = numpyString (fieldSize f) x
numpyBuild _ (Void      _) = mempty
numpyBuild f (Array     x) = V.foldMap (numpyBuild f) $ traverseTypeValue (pad . fromMaybe V.empty . getCompose) x where
   pad v = V.map Just (V.take l' v) V.++ V.replicate (l' - V.length v) Nothing
   l' = fromIntegral (fieldLength f)

numpyRowSize :: [Field] -> Word
numpyRowSize = sum . map nfs where
  nfs f@Field{ fieldType = Array _ } = fieldLength f * numpyFieldSize f
  nfs f = numpyFieldSize f

numpyHeader :: [Field] -> Word -> (B.Builder, Word)
numpyHeader fields count = (B.string8 "\147NUMPY"
  <> B.int8 (if vers2 then 2 else 1)
  <> B.int8 0
  <> (if vers2 then B.int32LE . fromIntegral else B.int16LE . fromIntegral) len'
  <> header
  <> stimesMonoid pad (B.char7 ' ')
  <> B.char7 '\n'
  , fromIntegral (plen + len') + count * numpyRowSize fields)
  where
  plen = 8 + if vers2 then 4 else 2
  header = "{'descr':["
    <> mintersperseMap (B.char7 ',') field fields 
    <> "],'fortran_order':False,'shape':(" <> B.wordDec count <> ",)}"
  field f = B.char7 '(' <> jenc (fieldName f) <> B.char7 ',' <> B.char7 '\'' <> B.string7 (numpyDtype f) <> B.char7 '\'' <> array f <> B.char7 ')'
  array f@Field{ fieldType = Array _ } = ",(" <> B.wordDec (fieldLength f) <> ",)"
  array _ = mempty
  len = succ $ BSL.length $ B.toLazyByteString header
  pad = negate $ (plen + len) `mod` (-64)
  len' = len + pad
  vers2 = len > 65472 -- with padding
  jenc = J.fromEncoding . J.toEncoding -- json is similar enough to python for most things

numpyValue :: Field -> J.Value -> B.Builder
numpyValue f = numpyBuild f . parseTypeJSONValue (fieldType f)

unconsJ :: [J.Value] -> (J.Value, [J.Value])
unconsJ [] = (J.Null, [])
unconsJ (j:l) = (j, l)

numpyRow :: [Field] -> [J.Value] -> B.Builder
numpyRow [] _ = mempty
numpyRow (f:fl) x = numpyValue f j <> numpyRow fl jl where (j, jl) = unconsJ x

numpyValueRow :: V.Vector Field -> V.Vector (TypeValue Maybe) -> B.Builder
numpyValueRow f v = fold $ V.zipWith numpyBuild f v

numpyGenerator :: Catalog -> DataArgs V.Vector -> M OutputStream
numpyGenerator cat args = do
  count <- queryCount cat (dataFilters args)
  let (header, size) = numpyHeader (V.toList $ dataFields args) count
  outputStreamRows
    (Just size)
    header
    (numpyValueRow $ dataFields args)
    mempty
    cat args

numpyOutput :: OutputFormat
numpyOutput = OutputFormat
  { outputMimeType = "application/x-npy"
  , outputExtension = "npy"
  , outputDescription = "Numpy binary array file containing a 1-d array of structured data types representing the fields in each row; missing values are generally represented by NaN or extreme values"
  , outputGenerator = \_ -> numpyGenerator
  }
