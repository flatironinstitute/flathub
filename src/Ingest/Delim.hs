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
import           Data.List (mapAccumL, find, findIndex, genericDrop, genericSplitAt)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word (Word64)
import           Text.Read (readMaybe)
import           System.IO (hFlush, stdout)

import Global
import Type
import Field
import Catalog
import Compression
import qualified ES
import Ingest.Types

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

ingestDelim :: Delim -> Ingest -> M Word64
ingestDelim delim info@Ingest{ ingestCatalog = cat, ingestOffset = off } = do
  ls <- liftIO $ map BSLC.toStrict . BSLC.lines <$> decompressFile (ingestFile info)
  let
    (header, rows) = parseHeaders delim ls
    (missing, fields) = mapAccumL (\c s -> maybe (c, Nothing) (\(f, c') -> (c', Just f)) $ takeCatalogField (TE.decodeUtf8 s) c) cat header
    rows' = genericDrop off $ filter (not . commentLine) rows
    key
      | Just i <- do
          n <- catalogKey cat 
          findIndex (any ((n ==) . fieldName)) fields
        = \_ x -> BSC.unpack $ x !! i
      | otherwise = \i _ -> ingestPrefix info ++ show i
    val _ Nothing _ = mempty
    val fx (Just f) x
      | typeIsFloating (fieldType f) && x `elem` ["Inf", "-Inf", "+Inf", "inf"] = mempty
      | BSC.null x = mempty
      | x `elem` fieldMissing f = mempty
      | otherwise = fieldName f J..= recode fx f x
    recode fx Field{ fieldIngest = Just (T.stripPrefix "scale:" -> Just sf), fieldScale = s } =
      J.toJSON . maybe id ((*) . realToFrac) s . (* (read $ BSC.unpack scale :: Double)) . read . BSC.unpack
      where Just (_, scale) = find (any ((sf ==) . fieldName) . fst) fx
    recode _ Field{ fieldScale = Just s } = J.toJSON . (*) s . read . BSC.unpack
    recode _ Field{ fieldType = (Boolean _) } = bool
    recode _ _ = J.String . TE.decodeLatin1
    bool "0" = J.Bool False
    bool "0.0" = J.Bool False
    bool "false" = J.Bool False
    bool "1" = J.Bool True
    bool "1.0" = J.Bool True
    bool "true" = J.Bool True
    bool s = J.String $ TE.decodeLatin1 s
    loop o [] = return o
    loop o s = do
      liftIO $ putStr (show o ++ "\r") >> hFlush stdout
      let (d, s') = genericSplitAt (ingestBlockSize info) s
          (o', block) = mapAccumL (\i l ->
              let x = splitDelim delim l
                  fx = zip fields x
              in (succ i, (key i x, ingestJConsts info <> foldMap (uncurry $ val fx) fx)))
            o d
      ES.createBulk cat block
      loop o' s'
  unless (HM.null $ catalogFieldMap missing) $ fail $ "missing fields: " ++ show (fieldName <$> catalogFieldMap missing)
  loop off rows'

ingestDat :: Ingest -> M Word64
ingestDat = ingestDelim (Delim ' ' False)

ingestTxt :: Ingest -> M Word64
ingestTxt = ingestDelim (Delim ' ' True)
