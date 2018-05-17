{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Query
  ( catalog
  , catalogBulk
  , BulkFormat(..)
  ) where

import           Control.Applicative (empty, (<|>))
import           Control.Monad (guard, unless)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Function (fix)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe, isNothing, listToMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Network.HTTP.Types.Header (hContentType, hContentDisposition)
import           Network.HTTP.Types.Status (ok200, badRequest400)
import qualified Network.Wai as Wai
import           Text.Read (readMaybe)
import           Waimwork.HTTP (quoteHTTP)
import           Waimwork.Response (response, okResponse)
import           Waimwork.Result (result)
import qualified Web.Route.Invertible as R

import Schema
import Global
import CSV
import qualified ES
#ifdef HAVE_pgsql
import qualified PG
#endif
import Compression

parseQuery :: Wai.Request -> Query
parseQuery = foldMap parseQueryItem . Wai.queryString where
  parseQueryItem ("offset", Just (readMaybe . BSC.unpack -> Just n)) =
    mempty{ queryOffset = n }
  parseQueryItem ("limit",  Just (readMaybe . BSC.unpack -> Just n)) =
    mempty{ queryLimit  = n }
  parseQueryItem ("sort",   Just s) =
    mempty{ querySort = map ps (BSC.splitWith delim s) } where
    ps f = case BSC.uncons f of
      Just ('+', r) -> (TE.decodeUtf8 r, True)
      Just ('-', r) -> (TE.decodeUtf8 r, False)
      _             -> (TE.decodeUtf8 f, True)
  parseQueryItem ("fields", Just s) =
    mempty{ queryFields = map TE.decodeUtf8 (BSC.splitWith delim s) }
  parseQueryItem ("aggs",   Just s) =
    mempty{ queryAggs = map TE.decodeUtf8 (BSC.splitWith delim s) }
  parseQueryItem ("hist",   Just (BSC.break (':' ==) -> (f, (BSC.uncons -> Just (':', i))))) =
    mempty{ queryHist = Just (TE.decodeUtf8 f, i) }
  parseQueryItem (f,        s) =
    mempty{ queryFilter = [(TE.decodeUtf8 f, a, snd <$> BS.uncons b)] } where
    (a, b) = BSC.break (',' ==) $ fromMaybe BS.empty s
  delim ',' = True
  delim ' ' = True
  delim _ = False

catalog :: Route Simulation
catalog = getPath (R.parameter R.>* "catalog") $ \sim req -> do
  cat <- askCatalog sim
  let query = fillQuery cat $ parseQuery req
  unless (queryLimit query <= 100) $
    result $ response badRequest400 [] ("limit too large" :: String)
  case catalogStore cat of
    CatalogES{} -> do
      res <- ES.queryIndex cat query
      return $ okResponse [] $ clean res
#ifdef HAVE_pgsql
    CatalogPG{} -> do
      res <- PG.queryTable cat query
      return $ okResponse [] res
#endif
  where
  clean = mapObject $ HM.mapMaybeWithKey cleanTop
  cleanTop "aggregations" = Just
  cleanTop "hits" = Just . mapObject (HM.mapMaybeWithKey cleanHits)
  cleanTop _ = const Nothing
  cleanHits "total" = Just
  cleanHits "hits" = Just . mapArray (V.map sourceOnly)
  cleanHits _ = const Nothing
  sourceOnly (J.Object o) = HM.lookupDefault J.emptyObject "_source" o
  sourceOnly v = v
  mapObject :: (J.Object -> J.Object) -> J.Value -> J.Value
  mapObject f (J.Object o) = J.Object (f o)
  mapObject _ v = v
  mapArray :: (V.Vector J.Value -> V.Vector J.Value) -> J.Value -> J.Value
  mapArray f (J.Array v) = J.Array (f v)
  mapArray _ v = v

data BulkFormat
  = BulkCSV
    { bulkCompression :: Maybe CompressionFormat
    }
  deriving (Eq)

instance R.Parameter R.PathString BulkFormat where
  parseParameter s = case decompressExtension (T.unpack s) of
    ("csv", z) -> return $ BulkCSV z
    _ -> empty
  renderParameter = T.pack . formatExtension

formatExtension :: BulkFormat -> String
formatExtension (BulkCSV z) = compressionFilename z "csv"

formatMimeType :: BulkFormat -> BS.ByteString
formatMimeType (BulkCSV z) = maybe "text/csv" compressionMimeType z

catalogBulk :: Route (Simulation, BulkFormat)
catalogBulk = getPath (R.parameter R.>*< R.parameter) $ \(sim, fmt) req -> do
  cat <- askCatalog sim
  let query = fillQuery cat $ parseQuery req
      enc = bulkCompression fmt <|> listToMaybe (acceptCompressionEncoding req)
  unless (queryOffset query == 0 && null (queryAggs query) && isNothing (queryHist query)) $
    result $ response badRequest400 [] ("offset,aggs not supported for download" :: String)
#ifdef HAVE_pgsql
  glob <- ask
#endif
  nextes <- ES.queryBulk cat query
  return $ Wai.responseStream ok200 (
    [ (hContentType, formatMimeType fmt)
    , (hContentDisposition, "attachment; filename=" <> quoteHTTP (TE.encodeUtf8 sim <> BSC.pack ('.' : formatExtension fmt)))
    ] ++ compressionEncodingHeader (guard (enc /= bulkCompression fmt) >> enc))
    $ compressStream enc $ \chunk _ -> do
    chunk $ csvTextRow $ queryFields query
    case catalogStore cat of
      CatalogES{} -> fix $ \loop -> do
        block <- nextes
        unless (V.null block) $ do
          chunk $ foldMap csvJSONRow block
          loop
#ifdef HAVE_pgsql
      CatalogPG{} -> runGlobal glob $ PG.queryBulk cat query $ \nextpg -> fix $ \loop -> do
        block <- nextpg
        unless (null block) $ do
          chunk $ foldMap csvJSONRow block
          loop
#endif

