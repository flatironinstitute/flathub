{-# LANGUAGE OverloadedStrings #-}

module Output.FITS
  -- ( fitsOutput )
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
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

blockLength :: Int
blockLength = 2880

class Render a where
  render :: a -> BS.ByteString

data HeaderValue
  = HeaderString BS.ByteString
  | HeaderLogical Bool
  | HeaderInteger Int
  -- other types not used

data HeaderRecord = HeaderRecord
  { headerName :: BS.ByteString
  , headerValue :: Maybe HeaderValue -- used fixed-format for all
  , headerComment :: Maybe BS.ByteString -- no continuation
  }

instance Render HeaderRecord where
  render (HeaderRecord n v c) =
    pad 8 n <> maybe (padding 2 <> pad 70 (fold c)) (("= " <>) . hv) v
    where
    hv (HeaderString s) = case spls $ esc s of
      [] -> comm BS.empty
      (f:r) -> comm (quot f) <> foldMap (("CONTINUE  " <>) . pad 70 . quot) r
    hv (HeaderLogical b) = comm (padR 20 (if b then "T" else "F"))
    hv (HeaderInteger i) = comm (padR 20 (BSC.pack $ show i))
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

instance Render [HeaderRecord] where
  render h = s <> padding (negate (BS.length s `mod` (negate blockLength)))
    where 
    s = foldMap render $ h ++ [ HeaderRecord "END" Nothing Nothing ]

axisHeaders :: Int -> [Int] -> [HeaderRecord]
axisHeaders bitpix naxis =
  [ HeaderRecord "BITPIX" (Just $ HeaderInteger bitpix) Nothing
  , HeaderRecord "NAXIS" (Just $ HeaderInteger $ length naxis) Nothing
  ] ++ zipWith (\i n -> HeaderRecord ("NAXIS" <> BSC.pack (show i)) (Just $ HeaderInteger n) Nothing) [(1::Int)..] naxis

primaryHeaders :: Int -> [Int] -> [HeaderRecord]
primaryHeaders bitpix naxis =
  [ HeaderRecord "SIMPLE" (Just $ HeaderLogical True) (Just "FITS")
  ]
  ++ axisHeaders bitpix naxis ++
  [ HeaderRecord "EXTEND" (Just $ HeaderLogical True) Nothing ]

