{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Ingest.Delim
  ( ingestDat
  , ingestTxt
  ) where

import           Control.Arrow (first)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Strict as HM
import           Data.List (mapAccumL, findIndex, genericDrop, genericSplitAt)
import           Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import           Data.Word (Word64)
import           Text.Read (readMaybe)
import           System.FilePath (dropExtension)
import           System.IO (hFlush, stdout)

import Global
import Field
import Catalog
import Compression
import qualified ES

data Delim = Delim
  { delimDelim :: !Char
  , delimMulti :: !Bool
  } deriving (Show)

splitDelim :: Delim -> BSC.ByteString -> [BSC.ByteString]
splitDelim Delim{ delimDelim = c, delimMulti = False } =                           BSC.split c
splitDelim Delim{ delimDelim = c, delimMulti = True }  = filter (not . BSC.null) . BSC.split c

commentHeaders :: Int -> [BSC.ByteString] -> ([BSC.ByteString], [BSC.ByteString])
commentHeaders _ [] = ([], [])
commentHeaders i hr@(h:r) = case BSC.uncons h of
  Just ('#', BSC.words -> ((readMaybe . BSC.unpack -> Just j):f:_)) | j == i -> first (f :) $ commentHeaders (succ i) r
  _ -> ([], hr)

parseHeaders :: Delim -> [BSC.ByteString] -> ([BSC.ByteString], [BSC.ByteString])
parseHeaders _ [] = ([], [])
parseHeaders d a@(h:r) = case BSC.uncons h of
  Just ('#', l) -> case BSC.words l of
    "0":_:_ -> commentHeaders (0 :: Int) a
    _ -> (splitDelim d l, r)
  _ ->   (splitDelim d h, r)

commentLine :: BSC.ByteString -> Bool
commentLine x = case BSC.uncons x of
  Just ('#', _) -> True
  Nothing -> True
  Just _ -> False

ingestDelim :: Delim -> Catalog -> J.Series -> Word64 -> FilePath -> Word64 -> M Word64
ingestDelim delim cat consts blockSize fn off = do
  ls <- liftIO $ map BSLC.toStrict . BSLC.lines <$> decompressFile fn
  let
    (header, rows) = parseHeaders delim ls
    (missing, fields) = mapAccumL (\c s -> maybe (c, Nothing) (\(f, c') -> (c', Just f)) $ takeCatalogField (TE.decodeUtf8 s) c) cat header
    rows' = genericDrop off $ filter (not . commentLine) rows
    key
      | Just i <- do
          n <- catalogKey cat 
          findIndex (any ((n ==) . fieldName)) fields
        = \_ x -> BSC.unpack $ x !! i
      | otherwise = \i _ -> fnb ++ '_' : show i
    val Nothing _ = mempty
    val (Just f) x
      | typeIsFloating (fieldType f) && x `elem` ["Inf", "-Inf", "+Inf"] = mempty
      | BSC.null x = mempty
      | otherwise = fieldName f J..= TE.decodeLatin1 x
    loop o [] = return o
    loop o s = do
      liftIO $ putStr (show o ++ "\r") >> hFlush stdout
      let (d, s') = genericSplitAt blockSize s
          (o', block) = mapAccumL (\i l -> let x = splitDelim delim l in (succ i, (key i x, consts <> foldMap (uncurry val) (zip fields x)))) o d
      ES.createBulk cat block
      loop o' s'
  unless (HM.null $ catalogFieldMap missing) $ fail $ "missing fields: " ++ show (fieldName <$> catalogFieldMap missing)
  loop off rows'
  where
  fnb = dropExtension fn

ingestDat :: Catalog -> J.Series -> Word64 -> FilePath -> Word64 -> M Word64
ingestDat = ingestDelim (Delim ' ' False)

ingestTxt :: Catalog -> J.Series -> Word64 -> FilePath -> Word64 -> M Word64
ingestTxt = ingestDelim (Delim ' ' True)
