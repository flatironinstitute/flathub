{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Output.FITS
  -- ( fitsOutput )
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (fold)
import           Data.Functor.Compose (getCompose)
import           Data.Maybe (fromMaybe, isJust, catMaybes)
import           Data.Monoid (Sum(Sum))
import           Data.Proxy (Proxy(Proxy), asProxyTypeOf)
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

padding :: Int -> BS.ByteString
padding n = BSC.replicate n ' '

pad :: Int -> BS.ByteString -> BS.ByteString
pad n s
  | BS.length s > n = error $ "FITS.pad " ++ show n ++ ": " ++ show s
  | otherwise = s <> padding (n - BS.length s)

padR :: Int -> BS.ByteString -> BS.ByteString
padR n s
  | BS.length s > n = error $ "FITS.padR " ++ show n ++ ": " ++ show s
  | otherwise = padding (n - BS.length s) <> s

blockSize :: Int
blockSize = 2880

blockPadding :: Int -> Int
blockPadding = negate . (`mod` negate blockSize)

showBS :: Show a => a -> BS.ByteString
showBS = BSC.pack . show

data HeaderValue
  = HeaderString BS.ByteString
  | HeaderLogical Bool
  | HeaderInteger Integer
  -- other types not used

data HeaderRecord = HeaderRecord
  { headerName :: BS.ByteString
  , headerValue :: Maybe HeaderValue -- used fixed-format for all
  , headerComment :: Maybe BS.ByteString -- no continuation
  }

renderRecord :: HeaderRecord -> BS.ByteString
renderRecord (HeaderRecord n v c) =
  pad 8 n <> maybe (padding 2 <> pad 70 (fold c)) (("= " <>) . hv) v
  where
  hv (HeaderString s) = case spls $ esc s of
    [] -> comm BS.empty
    (f:r) -> comm (quot f) <> foldMap (("CONTINUE  " <>) . pad 70 . quot) r
  hv (HeaderLogical b) = comm (padR 20 (if b then "T" else "F"))
  hv (HeaderInteger i) = comm (padR 20 (showBS i))
  comm s = pad 70 (s <> foldMap (" / " <>) c)
  quot s = ('\'' `BSC.cons` s) `BSC.snoc` '\''
  esc s = BSC.split '\'' s
  spls [] = []
  spls [x]
    | BS.length x <= 67 = [x]
    | otherwise = (a `BSC.snoc` '&') : spls [b] where (a, b) = BS.splitAt 67 x
  spls (x:y:r)
    | BS.length x < 66 = spls ((x <> "''" <> y):r)
    | otherwise = (a `BSC.snoc` '&') : spls (b : y : r) where (a, b) = BS.splitAt 67 x

renderBlock :: [HeaderRecord] -> BS.ByteString
renderBlock h = s <> padding (blockPadding (BS.length s)) where
  s = foldMap renderRecord $ h ++ [ HeaderRecord "END" Nothing Nothing ]

data TField = TField
  { tfCount :: Word
  , tfType :: Char
  , tfName :: Maybe BS.ByteString
  , tfUnit :: Maybe BS.ByteString
  -- , tfScale, tFieldZero :: Rational
  , tfNull :: Maybe Integer
  }

typeBytes :: Char -> Word
typeBytes 'L' = 1
typeBytes 'B' = 1
typeBytes 'I' = 2
typeBytes 'J' = 4
typeBytes 'K' = 8
typeBytes 'A' = 1
typeBytes 'E' = 4
typeBytes 'D' = 8
typeBytes 'C' = 8
typeBytes 'M' = 16
typeBytes 'P' = 8
typeBytes 'Q' = 16
typeBytes c = error $ "typeBytes " ++ show c

tfBytes :: TField -> Word
tfBytes tf = tfCount tf * typeBytes (tfType tf)

cField :: Char -> TField
cField t = TField
  { tfCount = 1
  , tfType = t
  , tfName = Nothing
  , tfUnit = Nothing
  -- , tfScale = 1
  -- , tfZero = 0
  , tfNull = Nothing
  }

cFieldInt :: (Integral a, Bounded a) => Char -> Proxy a -> TField
cFieldInt c p = (cField c){ tfNull = Just $ toInteger $ minBound `asProxyTypeOf` p }

typeInfo :: Type -> TField
typeInfo (Double    _) = cField 'D'
typeInfo (Float     _) = cField 'E'
typeInfo (HalfFloat _) = typeInfo (Float Proxy)
typeInfo (Long      p) = cFieldInt 'K' p
typeInfo (ULong     _) = (cField 'K'){- tfZero = 2^63 -}
typeInfo (Integer   p) = cFieldInt 'J' p
typeInfo (Short     p) = cFieldInt 'I' p
typeInfo (Byte      _) = (cField 'B'){- tfZero = -127 -}
typeInfo (Boolean   _) = cField 'L'
typeInfo (Keyword   _) = cField 'A' -- count set by size below
typeInfo (Void      _) = (cField 'P'){ tfCount = 0 }
typeInfo (Array     t) = typeInfo (arrayHead t) -- count set by length below

tField :: Field -> TField
tField f@Field{ fieldType = ft } = ti
  { tfCount = tfCount ti
      * (if typeIsString ft         then fieldSize f   else 1)
      * (if isJust (typeIsArray ft) then fieldLength f else 1)
  , tfName = Just $ TE.encodeUtf8 $ fieldName f -- XXX supposed to be ASCII -- will anyone care?
  , tfUnit = TE.encodeUtf8 <$> fieldUnits f
  } where ti = typeInfo ft

tFieldRecords :: Int -> TField -> [HeaderRecord]
tFieldRecords i TField{..} =
  [ rec "TFORM" $ HeaderString $ mwhen (tfCount /= 1) (showBS tfCount) `BSC.snoc` tfType
  ] ++ catMaybes
  [ rec "TTYPE" . HeaderString <$> tfName
  , rec "TUNIT" . HeaderString <$> tfUnit
  , rec "TNULL" . HeaderInteger <$> tfNull
  ]
  where
  is = BSC.pack $ show i
  rec n v = HeaderRecord (n <> is) (Just v) Nothing

axisHeaders :: Int -> [Word] -> [HeaderRecord]
axisHeaders bitpix naxis =
  [ HeaderRecord "BITPIX" (Just $ HeaderInteger $ toInteger bitpix) Nothing
  , HeaderRecord "NAXIS" (Just $ HeaderInteger $ toInteger $ length naxis) Nothing
  ] ++ zipWith (\i n -> HeaderRecord ("NAXIS" <> showBS i) (Just $ HeaderInteger $ toInteger n) Nothing) [(1::Int)..] naxis

fitsHeaders :: V.Vector Field -> Word -> (BS.ByteString, Word, BS.ByteString)
fitsHeaders fields count =
  ( renderBlock (
      [ HeaderRecord "SIMPLE" (Just $ HeaderLogical True) (Just "FITS")
      ] ++ axisHeaders 8 [] ++
      [ HeaderRecord "EXTEND" (Just $ HeaderLogical True) Nothing ])
    <> renderBlock (
      [ HeaderRecord "XTENSION" (Just $ HeaderString "BINTABLE") (Just "binary table")
      ] ++ axisHeaders 8 [rowsize, count] ++
      [ HeaderRecord "PCOUNT" (Just $ HeaderInteger 0) Nothing
      , HeaderRecord "GCOUNT" (Just $ HeaderInteger 1) Nothing
      , HeaderRecord "TFIELDS" (Just $ HeaderInteger $ toInteger $ V.length fields) Nothing
      ] ++ fold (V.imap (tFieldRecords . succ) tfields))
  , datasize, BS.replicate (blockPadding $ fromIntegral datasize) 0)
  where
  tfields = V.map tField fields
  Sum rowsize = V.foldMap (Sum . tfBytes) tfields
  datasize = count * rowsize
