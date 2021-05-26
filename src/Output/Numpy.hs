{-# LANGUAGE OverloadedStrings #-}

module Output.Numpy
  ( numpyHeader
  , numpyRow
  ) where

import qualified Data.Aeson.Types as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (stimesMonoid)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word (Word16, Word32, Word64)
import           Foreign.C.Types (CUShort(CUShort))
import           Numeric.Half (Half(Half))
import           Unsafe.Coerce (unsafeCoerce)

import Type
import Field
import Monoid

getHalf' :: Half -> Word16
getHalf' (Half (CUShort x)) = x

numpyString :: Int -> Maybe T.Text -> B.Builder
numpyString l t = B.byteString s <> stimesMonoid (l - BS.length s) (B.char7 '\0') where
  s = BS.take l $ foldMap TE.encodeUtf8 t

numpyBuild :: TypeValue Maybe -> B.Builder
numpyBuild (Double    x) = B.doubleLE $ fromMaybe (unsafeCoerce (0x7ff80000ffffffff::Word64)) x
numpyBuild (Float     x) = B.floatLE  $ fromMaybe (unsafeCoerce (0x7fc0ffff::Word32)) x
numpyBuild (HalfFloat x) = B.word16LE $ maybe 0x7cff getHalf' x
numpyBuild (Long      x) = B.int64LE  $ fromMaybe (-1) x
numpyBuild (Integer   x) = B.int32LE  $ fromMaybe (-1) x
numpyBuild (Short     x) = B.int16LE  $ fromMaybe (-1) x
numpyBuild (Byte      x) = B.int8     $ fromMaybe (-1) x
numpyBuild (Boolean Nothing) = B.int8 0
numpyBuild (Boolean (Just False)) = B.int8 0
numpyBuild (Boolean (Just True)) = B.int8 1
numpyBuild (Keyword   x) = numpyString 8 x
numpyBuild (Void      _) = mempty

numpyRowSize :: [Field] -> Word
numpyRowSize = sum . map (numpySize . fieldType)

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
  field f = B.char7 '(' <> jenc (fieldName f) <> B.char7 ',' <> B.char7 '\'' <> B.string7 (numpyDtype (fieldType f)) <> B.char7 '\'' <> B.char7 ')'
  len = succ $ BSL.length $ B.toLazyByteString header
  pad = negate $ (plen + len) `mod` (-64)
  len' = len + pad
  vers2 = len > 65472 -- with padding
  jenc = J.fromEncoding . J.toEncoding -- json is similar enough to python for most things

numpyValue :: Field -> J.Value -> B.Builder
numpyValue f = numpyBuild . parseTypeJSONValue (fieldType f)

unconsJ :: [J.Value] -> (J.Value, [J.Value])
unconsJ [] = (J.Null, [])
unconsJ (j:l) = (j, l)

numpyRow :: [Field] -> [J.Value] -> B.Builder
numpyRow [] _ = mempty
numpyRow (f:fl) x = numpyValue f j <> numpyRow fl jl where (j, jl) = unconsJ x
