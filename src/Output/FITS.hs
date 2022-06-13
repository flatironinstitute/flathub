{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Output.FITS
  ( fitsOutput )
  where

import           Control.Applicative ((<|>))
import           Control.Monad (join)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (toUpper)
import           Data.Foldable (fold)
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Maybe (fromMaybe, isJust, catMaybes)
import           Data.Monoid (Sum(Sum))
import           Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import           Data.Semigroup (stimesMonoid)
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock (getCurrentTime, UTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Vector as V
import           Data.Word (Word32, Word64)
import qualified Network.Wai as Wai
import           Numeric (showGFloat)
import           Unsafe.Coerce (unsafeCoerce)

import Type
import Field
import Catalog
import Monoid
import Output.Types
import Global
import Backend
import qualified KeyedMap as KM

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
  | HeaderDouble Double

data HeaderRecord = HeaderRecord
  { headerName :: BS.ByteString
  , headerValue :: Maybe HeaderValue -- used fixed-format for all
  , headerComment :: Maybe BS.ByteString
  }

zipLast :: (a -> b -> c) -> a -> a -> [b] -> [c]
zipLast _ _ _ [] = []
zipLast f _ z [x] = [f z x]
zipLast f a z (x:r) = f a x:zipLast f a z r

renderRecord :: HeaderRecord -> BS.ByteString
renderRecord HeaderRecord{..} =
  pad 8 headerName <> maybe ("  " <> commtail 0 (fold headerComment)) (("= " <>) . hv) headerValue
  where
  hv (HeaderString s) = case zipLast commval Nothing headerComment $ map quote $ spls $ esc s of
    ~(f:r) -> f <> foldMap ("CONTINUE  " <>) r
  hv (HeaderLogical b) = commval headerComment $ padR 20 (if b then "T" else "F")
  hv (HeaderInteger i) = commval headerComment $ padR 20 (showBS i)
  hv (HeaderDouble d) = commval headerComment $ padR 20 (BSC.pack $ map toUpper $ showGFloat (Just 10) d "")
  commval Nothing s = pad 70 s
  commval (Just c) s
    | BS.length s >= 67 = pad 70 s <> comment c
    | otherwise = s <> " / " <> commtail (BS.length s + 3) c
  commtail u s = pad p f <> comment r where
    (f, r) = BS.splitAt p s
    p = 70 - u
  comment s
    | BS.null s = BS.empty
    | otherwise = "COMMENT   " <> commtail 0 s
  quote s = ('\'' `BSC.cons` s) `BSC.snoc` '\''
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
  , tfTypeComment, tfName, tfNameComment, tfUnit :: Maybe BS.ByteString
  -- , tfScale, tFieldZero :: Rational
  , tfNull :: Maybe Integer
  , tfMin, tfMax :: Maybe Double
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
  , tfTypeComment = Nothing
  , tfName = Nothing
  , tfNameComment = Nothing
  , tfUnit = Nothing
  -- , tfScale = 1
  -- , tfZero = 0
  , tfNull = Nothing
  , tfMin = Nothing
  , tfMax = Nothing
  }

cFieldInt :: (Integral a, Bounded a) => Char -> Proxy a -> TField
cFieldInt c p = (cField c){ tfNull = Just $ toInteger $ minBound `asProxyTypeOf` p }

typeInfo :: Type -> TField
typeInfo (Double    _) = cField 'D'
typeInfo (Float     _) = cField 'E'
typeInfo (HalfFloat _) = typeInfo (Float Proxy)
typeInfo (Long      p) = cFieldInt 'K' p
typeInfo (ULong     _) = (cField 'K'){ tfNull = Just (-1), tfTypeComment = Just "unsigned" {- tfZero = 2^63 -} }
typeInfo (Integer   p) = cFieldInt 'J' p
typeInfo (Short     p) = cFieldInt 'I' p
typeInfo (Byte      _) = (cField 'B'){ tfNull = Just 128, tfTypeComment = Just "signed" {- tfZero = -127 -} }
typeInfo (Boolean   _) = cField 'L'
typeInfo (Keyword   _) = cField 'A' -- count set by size below
typeInfo (Void      _) = (cField 'P'){ tfCount = 0 }
typeInfo (Array     t) = typeInfo (arrayHead t) -- count set by length below

tField :: FieldSub FieldFilter Proxy -> TField
tField f@Field{ fieldType = ft } = ti
  { tfCount = tfCount ti
      * (if typeIsString ft         then fieldSize f   else 1)
      * (if isJust (typeIsArray ft) then fieldLength f else 1)
  -- XXX supposed to be ASCII -- will anyone care?
  , tfTypeComment = BS.intercalate "," . map TE.encodeUtf8 . V.toList <$> fieldEnum f <|> tfTypeComment ti
  , tfName = Just $ TE.encodeUtf8 $ fieldName f
  , tfNameComment = Just $ TE.encodeUtf8 $ fieldTitle f <> foldMap (": " <>) (fieldDescr f)
  , tfUnit = TE.encodeUtf8 <$> fieldUnits f
  , tfMin = tmin
  , tfMax = tmax
  } where
  ti = typeInfo $ typeOfValue ft
  (tmin, tmax) = unTypeValue rf ft
  rf :: Typed a => FieldFilter a -> (Maybe Double, Maybe Double)
  rf (FieldRange x y) = (toDouble <$> x, toDouble <$> y)
  rf (FieldEQ [x]) = join (,) $ Just $ toDouble x
  rf _ = (Nothing, Nothing)

tFieldRecords :: Int -> TField -> [HeaderRecord]
tFieldRecords i TField{..} =
  [ rec "TFORM" tfTypeComment $ HeaderString $ mwhen (tfCount /= 1) (showBS tfCount) `BSC.snoc` tfType
  ] ++ catMaybes
  [ rec "TTYPE" tfNameComment . HeaderString <$> tfName
  , rec "TUNIT" Nothing . HeaderString <$> tfUnit
  , rec "TNULL" Nothing . HeaderInteger <$> tfNull
  , rec "TDMIN" Nothing . HeaderDouble <$> tfMin
  , rec "TDMAX" Nothing . HeaderDouble <$> tfMax
  ]
  where
  is = BSC.pack $ show i
  rec n c v = HeaderRecord (n <> is) (Just v) c

axisHeaders :: Int -> [Word] -> [HeaderRecord]
axisHeaders bitpix naxis =
  [ HeaderRecord "BITPIX" (Just $ HeaderInteger $ toInteger bitpix) Nothing
  , HeaderRecord "NAXIS" (Just $ HeaderInteger $ toInteger $ length naxis) Nothing
  ] ++ zipWith (\i n -> HeaderRecord ("NAXIS" <> showBS i) (Just $ HeaderInteger $ toInteger n) Nothing) [(1::Int)..] naxis

fitsHeaders :: UTCTime -> Wai.Request -> Catalog -> DataArgs V.Vector -> Word -> (BS.ByteString, Word, BS.ByteString)
fitsHeaders date req cat args count =
  ( renderBlock (
      [ HeaderRecord "SIMPLE" (Just $ HeaderLogical True) (Just "FITS")
      ] ++ axisHeaders 8 [] ++
      [ HeaderRecord "DATE" (Just $ HeaderString $ BSC.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" date) (Just "time of download")
      , HeaderRecord "ORIGIN" (Just $ HeaderString $ BSL.toStrict $ B.toLazyByteString $ requestUrl req) (Just $ TE.encodeUtf8 $ catalogTitle cat)
      , HeaderRecord "EXTEND" (Just $ HeaderLogical True) Nothing ])
    <> renderBlock (
      [ HeaderRecord "XTENSION" (Just $ HeaderString "BINTABLE") (Just "binary table")
      ] ++ axisHeaders 8 [rowsize, count] ++
      [ HeaderRecord "PCOUNT" (Just $ HeaderInteger 0) Nothing
      , HeaderRecord "GCOUNT" (Just $ HeaderInteger 1) Nothing
      , HeaderRecord "TFIELDS" (Just $ HeaderInteger $ toInteger $ V.length $ dataFields args) Nothing
      ] ++ fold (V.imap (tFieldRecords  . succ) tfields))
  , datasize, BS.replicate (blockPadding $ fromIntegral datasize) 0)
  where
  tfields = V.map (\f -> tField (KM.lookupDefault
      (runIdentity $ updateFieldValueM f (\_ -> Identity $ FieldEQ []))
      $ filterFields $ dataFilters args))
    $ dataFields args
  Sum rowsize = V.foldMap (Sum . tfBytes) tfields
  datasize = count * rowsize

fitsValue :: Field -> TypeValue Maybe -> B.Builder
fitsValue _ (Double    x) = B.doubleBE $ fromMaybe (unsafeCoerce (0x7ff80000ffffffff::Word64)) x
fitsValue _ (Float     x) = B.floatBE  $ fromMaybe (unsafeCoerce (0x7fc0ffff::Word32)) x
fitsValue f (HalfFloat x) = fitsValue f (Float $ realToFrac <$> x)
fitsValue _ (Long      x) = B.int64BE  $ fromMaybe minBound x
fitsValue _ (ULong     x) = B.word64BE $ fromMaybe maxBound x -- should be signed with zero shift
fitsValue _ (Integer   x) = B.int32BE  $ fromMaybe minBound x
fitsValue _ (Short     x) = B.int16BE  $ fromMaybe minBound x
fitsValue _ (Byte      x) = B.int8     $ fromMaybe minBound x -- should be unsigned with zero shift
fitsValue _ (Boolean Nothing) = B.int8 0
fitsValue _ (Boolean (Just False)) = B.char7 'F'
fitsValue _ (Boolean (Just True)) = B.char7 'T'
fitsValue f (Keyword   x) = B.byteString s <> stimesMonoid (l' - BS.length s) (B.char7 '\0') where
  s = BS.take l' $ foldMap TE.encodeUtf8 x -- supposed to be ASCII oh well
  l' = fromIntegral (fieldSize f)
fitsValue _ (Void      _) = mempty
fitsValue f (Array     x) = V.foldMap (fitsValue f) $ traverseTypeValue (padv . fromMaybe V.empty . getCompose) x where
   padv v = V.map Just (V.take l' v) V.++ V.replicate (l' - V.length v) Nothing
   l' = fromIntegral (fieldLength f)

fitsRow :: V.Vector Field -> V.Vector (TypeValue Maybe) -> B.Builder
fitsRow f v = fold $ V.zipWith fitsValue f v

fitsGenerator :: Wai.Request -> Catalog -> DataArgs V.Vector -> M OutputStream
fitsGenerator req cat args = do
  count <- queryCount cat (dataFilters args)
  now <- liftIO getCurrentTime
  let (header, size, footer) = fitsHeaders now req cat args count
  outputStreamRows
    (Just $ fromIntegral (BS.length header) + size + fromIntegral (BS.length footer))
    (B.byteString header)
    (fitsRow $ dataFields args)
    (B.byteString footer)
    cat args

fitsOutput :: OutputFormat
fitsOutput = OutputFormat
  { outputMimeType = "application/fits"
  , outputExtension = "fits"
  , outputDescription = "FITS BINTABLE binary table file containing the requested fields"
  , outputGenerator = fitsGenerator
  }
