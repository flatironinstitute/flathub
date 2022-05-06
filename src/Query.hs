{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Query
  ( parseQuery
  , catalog
  , catalogBulk
  , BulkFormat(..)
  ) where

import           Control.Applicative ((<|>), liftA2)
import           Control.Monad (guard, unless, mfilter)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import           Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable (fold)
import           Data.Function (on)
import qualified Data.HashMap.Strict as HM
import           Data.List (foldl', unionBy)
import           Data.Maybe (listToMaybe, maybeToList, isJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Network.HTTP.Types.Header (hContentType, hContentDisposition, hContentLength, hCacheControl)
import           Network.HTTP.Types.Status (ok200, badRequest400)
import qualified Network.Wai as Wai
import           Text.Read (readMaybe)
import           Waimwork.HTTP (quoteHTTP)
import           Waimwork.Response (response, okResponse)
import           Waimwork.Result (result)
import           System.FilePath ((<.>), splitExtension)
import qualified Web.Route.Invertible as R
import           Web.Route.Invertible.Render (renderUrlRequestBuilder)
import           Web.Route.Invertible.Internal (requestRoute')
import           Web.Route.Invertible.Wai (waiRequest)

import Type
import Field
import Catalog
import Global
import Output.CSV
import Output.ECSV
import qualified ES
import Compression
import Output.Numpy
import Monoid
import Attach

parseQuery :: Catalog -> Wai.Request -> Query
parseQuery cat req = fill $ foldl' parseQueryItem mempty $ Wai.queryString req where
  parseQueryItem q ("offset", Just (rmbs -> Just n)) =
    q{ queryOffset = queryOffset q + n }
  parseQueryItem q ("limit",  Just (rmbs -> Just n)) =
    q{ queryLimit  = queryLimit q `max` n }
  parseQueryItem q ("sort",   Just (mapM parseSort . spld -> Just s)) =
    q{ querySort = querySort q <> s }
  parseQueryItem q ("fields", Just (mapM lookf . spld -> Just l)) =
    q{ queryFields = unionBy ((==) `on` fieldName) (queryFields q) l }
  parseQueryItem q ("sample", Just (rmbs -> Just p)) =
    q{ querySample = querySample q * p }
  parseQueryItem q ("sample", Just (spl ('@' ==) -> Just (rmbs -> Just p, rmbs -> Just s))) =
    q{ querySample = querySample q * p, querySeed = Just $ maybe id xor (querySeed q) s }
  parseQueryItem q ("aggs",   Just (mapM lookf . spld -> Just a)) =
    q{ queryAggs = unionBy eqAgg (queryAggs q) $ map QueryStats a }
  parseQueryItem q ("hist",   Just (parseHist . spld -> Just h)) =
    q{ queryAggs = queryAggs q <> h }
  parseQueryItem q (lookf -> Just f, Just (parseFilt f -> Just v)) =
    q{ queryFilter = queryFilter q <> [liftFilterValue f v] }
  parseQueryItem q _ = q -- just ignore anything we can't parse
  parseSort (BSC.uncons -> Just ('+', lookf -> Just f)) = return (f, True)
  parseSort (BSC.uncons -> Just ('-', lookf -> Just f)) = return (f, False)
  parseSort (lookf -> Just f) = return (f, True)
  parseSort _ = fail "invalid sort"
  parseHist [] = return []
  parseHist ((spl (':' ==) -> Just (lookf -> Just f, histn -> Just n)) : l) = mkHist f n =<< parseHist l
  parseHist ((                      lookf -> Just f                  ) :[]) = return [QueryPercentiles f [0,25,50,75,100]]
  parseHist ((                      lookf -> Just f                  ) : l) = mkHist f (False, 16) =<< parseHist l
  parseHist _ = fail "invalid hist"
  histn s = (t, ) <$> rmbs r where
    (t, r) = maybe (False, s) (True ,) $ BS.stripPrefix "log" s
  parseFilt f (spl delim -> Just (a, b))
    | not (typeIsString (fieldType f)) = FilterRange <$> parseVal f a <*> parseVal f b
  parseFilt f a = FilterEQ <$> parseVal' f a
  parseVal _ "" = return Nothing
  parseVal f v = Just <$> parseVal' f v
  parseVal' f = fmap fieldType . parseFieldValue f . TE.decodeUtf8
  eqAgg (QueryStats f) (QueryStats g) = fieldName f == fieldName g
  eqAgg _ _ = False
  mkHist f (t, n) l
    | typeIsNumeric (fieldType f) = return $ [QueryHist f n t l]
    | otherwise = fail "non-numeric hist"
  fill q@Query{ queryFields = [] } | all (("fields" /=) . fst) (Wai.queryString req) =
    q{ queryFields = filter fieldDisp $ catalogFields cat }
  fill q = q
  spld = BSC.splitWith delim
  delim ',' = True
  delim ' ' = True
  delim _ = False
  rmbs :: Read a => BSC.ByteString -> Maybe a
  rmbs = readMaybe . BSC.unpack
  spl c s = (,) p . snd <$> BSC.uncons r
    where (p, r) = BSC.break c s
  lookf n = HM.lookup (TE.decodeUtf8 n) $ catalogFieldMap cat

catalog :: Route Simulation
catalog = getPath (R.parameter R.>* "catalog") $ \sim req -> do
  cat <- askCatalog sim
  let query = parseQuery cat req
      hsize = histsSize $ queryAggs query
  unless (queryLimit query <= 5000) $
    result $ response badRequest400 [] ("limit too large" :: String)
  unless (hsize > 0 && hsize <= 1024) $
    result $ response badRequest400 [] ("hist too large" :: String)
  res <- ES.queryIndex cat query
  return $ okResponse [] $ clean res
  where
  histSize (QueryHist _ n _ l) = n * histsSize l
  histSize _ = 1
  histsSize = product . map histSize
  clean = mapObject $ HM.mapMaybeWithKey cleanTop
  cleanTop "aggregations" = Just
  cleanTop "histsize" = Just
  cleanTop "hits" = Just . mapObject (HM.mapMaybeWithKey cleanHits)
  cleanTop _ = const Nothing
  cleanHits "total" (J.Object o) = HM.lookup "value" o
  cleanHits "total" v = Just v
  cleanHits "hits" a = Just $ mapArray (V.map cleanHit) a
  cleanHits _ _ = Nothing
  cleanHit (J.Object o) = J.Object $ ES.storedFields' o
  cleanHit v = v
  mapObject :: (J.Object -> J.Object) -> J.Value -> J.Value
  mapObject f (J.Object o) = J.Object (f o)
  mapObject _ v = v
  mapArray :: (V.Vector J.Value -> V.Vector J.Value) -> J.Value -> J.Value
  mapArray f (J.Array v) = J.Array (f v)
  mapArray _ v = v

data BulkFormat
  = BulkCSV (Maybe CompressionFormat)
  | BulkECSV (Maybe CompressionFormat)
  | BulkNumpy (Maybe CompressionFormat)
  | BulkJSON (Maybe CompressionFormat)
  | BulkAttachments (Maybe T.Text)
  | BulkAttachmentList (Maybe CompressionFormat) (Maybe T.Text)
  | BulkAttachmentScript (Maybe CompressionFormat) (Maybe T.Text)
  deriving (Eq)

attachmentExtension :: String -> Maybe CompressionFormat -> Maybe T.Text -> Maybe BulkFormat
attachmentExtension ".zip" Nothing = Just . BulkAttachments
attachmentExtension ".uris" z = Just . BulkAttachmentList z
attachmentExtension ".sh" z = Just . BulkAttachmentScript z
attachmentExtension _ _ = const Nothing

instance R.Parameter R.PathString BulkFormat where
  parseParameter s = case decompressExtension (T.unpack s) of
    ("csv", z) -> return $ BulkCSV z
    ("ecsv", z) -> return $ BulkECSV z
    ("npy", z) -> return $ BulkNumpy z
    ("numpy", z) -> return $ BulkNumpy z
    ("json", z) -> return $ BulkJSON z
    (a, z) ->
      let (n, e) = splitExtension a in
      attachmentExtension e z (mfilter ("attachments" /=) $ Just $ T.pack n)
  renderParameter = T.pack . formatExtension

formatExtension :: BulkFormat -> String
formatExtension b = bulkExtension $ bulk b undefined undefined undefined undefined

data Bulk = Bulk
  { bulkMimeType :: !BS.ByteString
  , bulkExtension :: !String
  , bulkCompression :: Maybe CompressionFormat
  , bulkQuery :: Query
  , bulkGenerator :: BulkGenerator
  }

type BulkGenerator = IO (Word, V.Vector J.Object) -> M BulkStream

data BulkStream = BulkStream
  { bulkSize :: Maybe Word
  , bulkStream :: (B.Builder -> IO ()) -> IO ()
  }

data BulkBlockStream r = BulkBlockStream
  { bbsSize :: Maybe Word
  , bbsHeader :: B.Builder
  , bbsRow :: r -> B.Builder
  , bbsFooter :: B.Builder
  }

instance Semigroup (BulkBlockStream r) where
  a <> b = BulkBlockStream
    { bbsSize = liftA2 (+) (bbsSize a) (bbsSize b)
    , bbsHeader = bbsHeader a <> bbsHeader b
    , bbsRow = \r -> bbsRow a r <> bbsRow b r
    , bbsFooter = bbsFooter a <> bbsFooter b
    }

instance Monoid (BulkBlockStream r) where
  mempty = BulkBlockStream
    { bbsSize = Nothing
    , bbsHeader = mempty
    , bbsRow = const mempty
    , bbsFooter = mempty
    }

bulkBlockGenerator :: Query -> (Word -> BulkBlockStream [J.Value]) -> BulkGenerator
bulkBlockGenerator query bbs next = do
  (count, block1) <- liftIO next
  let BulkBlockStream{..} = bbs count
      fields = map fieldName $ queryFields query
  return BulkStream
    { bulkSize = bbsSize
    , bulkStream = \chunk -> do
      chunk bbsHeader
      let loop block = unless (V.null block) $ do
            chunk $ foldMap (\o -> bbsRow $ map (\f -> HM.lookupDefault J.Null f o) fields) block
            loop . snd =<< next
      loop block1
      chunk bbsFooter
    }

compressBulk :: Maybe CompressionFormat -> Bulk -> Bulk
compressBulk Nothing b = b
compressBulk mz@(Just z) b = b
  { bulkMimeType = compressionMimeType z
  , bulkExtension = bulkExtension b <.> compressionExtension z
  , bulkCompression = mz
  , bulkGenerator = \next -> do
    s <- bulkGenerator b next
    return s{ bulkSize = Nothing }
  }

bulkBlock :: Maybe CompressionFormat -> Query -> BS.ByteString -> String -> (Word -> BulkBlockStream [J.Value]) -> Bulk
bulkBlock z query mt ext bbs = compressBulk z Bulk
  { bulkMimeType = mt
  , bulkExtension = ext
  , bulkCompression = Nothing
  , bulkQuery = query
  , bulkGenerator = bulkBlockGenerator query bbs
  }

attachmentQuery :: Catalog -> Maybe [T.Text] -> Query -> Query
attachmentQuery cat atn query = query
  { queryFields = ats
  , queryFilter = (case ats of
    [x@Field{ fieldType = Boolean _ }] -> (x{ fieldType = Boolean (FilterEQ True) } :)
    -- TODO could do better with OR filters
    _ -> id) $ queryFilter query
  } where
  ats = filter (isJust . fieldAttachment) $ maybe (queryFields query) (mapMaybe (`HM.lookup` catalogFieldMap cat)) atn

attachmentExists :: J.Value -> Bool
attachmentExists (J.Bool True) = True
attachmentExists (J.Number n) = n > 0
attachmentExists _ = False

bulkAttachmentUrls :: Maybe T.Text -> Maybe CompressionFormat -> Simulation -> Catalog -> Wai.Request -> Query -> BS.ByteString -> String -> BulkBlockStream B.Builder -> Bulk
bulkAttachmentUrls a z sim cat req query mt ext bbs = compressBulk z Bulk
  { bulkMimeType = mt
  , bulkExtension = maybe "attachments" T.unpack a <> ext
  , bulkCompression = Nothing
  , bulkQuery = attachmentQuery cat (fmap return a) query
  , bulkGenerator = bulkBlockGenerator query{ queryFields = idField : queryFields query } $ const bbs
    { bbsRow = \(J.String i:j) -> fold $ zipWith
      (\f v -> mwhen (attachmentExists v) $
        bbsRow bbs $ renderUrlRequestBuilder
          (requestRoute' (R.actionRoute attachment) (sim, fieldName f, i) (waiRequest req) { R.requestQuery = mempty })
          mempty)
      (queryFields query) j
    }
  }

bulk :: BulkFormat -> Simulation -> Catalog -> Wai.Request -> Query -> Bulk
bulk (BulkCSV z) _ _ _ query = bulkBlock z query
  "text/csv" "csv" $ const mempty
  { bbsHeader = csvTextRow $ map fieldName $ queryFields query
  , bbsRow = csvJSONRow
  }
bulk (BulkECSV z) _ cat _ query = bulkBlock z query
  "text/x-ecsv" "ecsv" $ const mempty
  { bbsHeader = ecsvHeader cat query
  , bbsRow = csvJSONRow
  }
bulk (BulkNumpy z) _ _ _ query = bulkBlock z query
  "application/octet-stream" "npy" $ \count ->
  let (header, size) = numpyHeader (queryFields query) count in
  mempty
  { bbsSize = Just size
  , bbsHeader = header
  , bbsRow = numpyRow $ queryFields query
  }
bulk (BulkJSON z) _ _ _ query = bulkBlock z query
  "application/json" "json" $ const mempty
  { bbsHeader = "[" <> J.fromEncoding (J.toEncoding (map fieldName (queryFields query)))
  , bbsRow = \j -> "," <> J.fromEncoding (J.toEncoding j)
  , bbsFooter = "]"
  }
bulk (BulkAttachments a) _ cat req query = Bulk
  { bulkMimeType = "application/zip"
  , bulkExtension = maybe "attachments" T.unpack a <> ".zip"
  , bulkCompression = Nothing
  , bulkQuery = query'{ queryFields = attachmentsFields cat att ++ ats }
  , bulkGenerator = \next -> BulkStream Nothing <$> attachmentsBulkStream info ats next
  }
  where
  query' = attachmentQuery cat (fmap return a) query
  ats = queryFields query'
  att = mapMaybe fieldAttachment ats
  info = TE.encodeUtf8 (catalogTitle cat <> (foldMap (T.cons ' ') a)) <> " downloaded from " <> Wai.rawPathInfo req
bulk (BulkAttachmentList z a) sim cat req query = bulkAttachmentUrls a z sim cat req query
  "text/uri-list" ".uris" $ mempty
  { bbsHeader = "# " <> TE.encodeUtf8Builder (catalogTitle cat) <> " attachments " <> foldMap TE.encodeUtf8Builder a <> "\n"
  , bbsRow = (<> "\n")
  }
bulk (BulkAttachmentScript z a) sim cat req query = bulkAttachmentUrls a z sim cat req query
  "text/x-shellscript" ".sh" $ mempty
  { bbsHeader = "#!/bin/sh\n# " <> TE.encodeUtf8Builder (catalogTitle cat) <> " attachments " <> foldMap TE.encodeUtf8Builder a <> "\n"
  , bbsRow = \u -> "curl -JO " <> u <> "\n"
  }

catalogBulk :: Route (Simulation, BulkFormat)
catalogBulk = getPath (R.parameter R.>*< R.parameter) $ \(sim, fmt) req -> do
  cat <- askCatalog sim
  let Bulk{..} = bulk fmt sim cat req $ parseQuery cat req
      enc = bulkCompression <|> listToMaybe (acceptCompressionEncoding req)
  nextes <- ES.queryBulk cat bulkQuery
  BulkStream{..} <- bulkGenerator nextes
  return $ Wai.responseStream ok200 (
    [ (hContentType, bulkMimeType)
    , (hContentDisposition, "attachment; filename=" <> quoteHTTP (TE.encodeUtf8 sim <> BSC.pack ('.' : bulkExtension)))
    , (hCacheControl, "public, max-age=86400")
    ]
    ++ maybeToList ((,) hContentLength . BSC.pack . show <$> bulkSize)
    ++ compressionEncodingHeader (guard (enc /= bulkCompression) >> enc))
    $ compressStream enc $ \chunk _ -> bulkStream chunk
