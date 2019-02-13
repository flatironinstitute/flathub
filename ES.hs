{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module ES
  ( initServer
  , createIndex
  , checkIndices
  , queryIndex
  , queryBulk
  , createBulk
  , flushIndex
  , countIndex
  , closeIndex
  ) where

import           Control.Monad ((<=<), forM, forM_, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J (Parser, parseEither, parseMaybe)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Default (def)
import           Data.Either (partitionEithers)
import           Data.Foldable (fold)
import           Data.Functor.Identity (Identity(Identity))
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.List (find)
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(Proxy))
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Header (hAccept, hContentType)
import           Network.HTTP.Types.Method (StdMethod(GET, PUT, POST), renderStdMethod)
import qualified Network.HTTP.Types.URI as HTTP (Query)
import qualified Network.URI as URI
import           Text.Read (readEither)
import qualified Waimwork.Config as C

import Monoid
import JSON
import Field
import Catalog
import Global

initServer :: C.Config -> IO HTTP.Request
initServer conf = HTTP.parseUrlThrow (conf C.! "server")

class Body a where
  bodyRequest :: a -> HTTP.RequestBody
  bodyContentType :: a -> Maybe BS.ByteString

instance Body () where
  bodyRequest _ = HTTP.requestBody HTTP.defaultRequest
  bodyContentType _ = Nothing

instance Body J.Encoding where
  bodyRequest = HTTP.RequestBodyLBS . JE.encodingToLazyByteString
  bodyContentType _ = Just "application/json"

instance Body J.Value where
  bodyRequest = bodyRequest . J.toEncoding
  bodyContentType _ = Just "application/json"

instance Body B.Builder where
  bodyRequest = HTTP.RequestBodyLBS . B.toLazyByteString
  bodyContentType _ = Just "application/x-ndjson"

instance Body EmptyJSON where
  bodyRequest EmptyJSON = HTTP.RequestBodyBS "{}"
  bodyContentType _ = Just "application/json"

elasticSearch :: (Body b, J.FromJSON r, Show r) => StdMethod -> [String] -> HTTP.Query -> b -> M r
elasticSearch meth url query body = do
  glob <- ask
  let req = globalES glob
      req' = HTTP.setQueryString query req
        { HTTP.method = renderStdMethod meth
        , HTTP.path = HTTP.path req <> BS.intercalate "/" (map (BSC.pack . URI.escapeURIString URI.isUnescapedInURIComponent) url)
        , HTTP.requestHeaders = maybe id ((:) . (,) hContentType) (bodyContentType body)
            $ (hAccept, "application/json")
            : HTTP.requestHeaders req
        , HTTP.requestBody = bodyRequest body
        }
  liftIO $ do
    -- print $ HTTP.path req'
    -- print $ JE.encodingToLazyByteString <$> body
    r <- either fail return . (J.parseEither J.parseJSON <=< AP.eitherResult)
      =<< HTTP.withResponse req' (globalHTTP glob) parse
    -- print r
    return r
  where
  parse r = AP.parseWith (HTTP.responseBody r) J.json BS.empty

catalogURL :: Catalog -> [String]
catalogURL Catalog{ catalogStore = ~CatalogES{ catalogIndex = idxn, catalogMapping = mapn } } =
  [T.unpack idxn, T.unpack mapn]

defaultSettings :: Catalog -> J.Object
defaultSettings cat = HM.fromList
  [ "index" J..= J.object
    [ "number_of_shards" J..= (clusterSize * min 100 (maybe 2 (succ . (`div` (clusterSize * docsPerShard))) (catalogCount cat)))
    , "number_of_replicas" J..= J.Number 1
    , "refresh_interval" J..= J.Number (-1)
    , "max_docvalue_fields_search" J..= (8 + length (catalogFields cat))
    ]
  ] where
  -- elastic search cluster size to optimize for (should really be determined dynamicaly)
  clusterSize = 4
  -- target number of docs per shard
  docsPerShard = 100000000

createIndex :: Catalog -> M J.Value
createIndex cat@Catalog{ catalogStore = ~CatalogES{..} } = elasticSearch PUT [T.unpack catalogIndex] [] $ JE.pairs $
     "settings" J..= mergeJSONObject catalogSettings (defaultSettings cat)
  <> "mappings" .=*
    (  catalogMapping .=*
      (  "dynamic" J..= J.String "strict"
      <> "_source" .=* ("enabled" J..= (catalogStoreField == ESStoreSource))
      <> "properties" J..= HM.map field (catalogFieldMap cat)))
  where
  field f = J.object
    [ "type" J..= (fieldType f :: Type)
    , "store" J..= (catalogStoreField == ESStoreStore)
    ]

checkIndices :: M (HM.HashMap Simulation String)
checkIndices = do
  isdev <- asks globalDevMode
  indices <- elasticSearch GET ["*"] [] ()
  HM.mapMaybe (\cat -> either Just (const Nothing) $ J.parseEither (catalog isdev cat) indices)
    <$> asks (catalogMap . globalCatalogs)
  where
  catalog isdev ~cat@Catalog{ catalogStore = CatalogES{ catalogIndex = idxn, catalogMapping = mapn } } =
    parseJSONField idxn (idx isdev cat mapn)
  idx :: Bool -> Catalog -> T.Text -> J.Value -> J.Parser ()
  idx isdev cat mapn = J.withObject "index" $ \i -> do
    sets <- i J..: "settings" >>= (J..: "index")
    ro <- or <$> (sets J..:? "blocks" >>= maybe (return Nothing) (J..:? "read_only"))
    unless (isdev || ro) $ fail "open (not read_only)"
    parseJSONField "mappings" (J.withObject "mappings" $
      parseJSONField mapn (mapping $ catalogFields cat)) i
  mapping :: Fields -> J.Value -> J.Parser ()
  mapping fields = J.withObject "mapping" $ parseJSONField "properties" $ J.withObject "properties" $ \ps ->
    forM_ fields $ \field -> parseJSONField (fieldName field) (prop field) ps
  prop :: Field -> J.Value -> J.Parser ()
  prop field = J.withObject "property" $ \p -> do
    t <- p J..: "type"
    unless (t == fieldType field) $ fail $ "incorrect field type; should be " ++ show (fieldType field)

scrollTime :: IsString s => s
scrollTime = "60s"

queryIndexScroll :: Bool -> Catalog -> Query -> M J.Value
queryIndexScroll scroll cat@Catalog{ catalogStore = ~CatalogES{ catalogStoreField = store } } Query{..} = do
  hists <- mapMaybe histsize . (histbnds ++) <$> if null histunks then return [] else
    fold . J.parseMaybe fillhistbnd <$> elasticSearch GET
      (catalogURL cat ++ ["_search"]) []
      (JE.pairs $
        "size" J..= J.Number 0
      <> "query" .=* filts
      <> "aggs" .=* foldMap (\(f, _) -> 
           ("0" <> fieldName f) .=* ("min" .=* field f)
        <> ("1" <> fieldName f) .=* ("max" .=* field f))
        histunks)
  amend hists <$> elasticSearch GET
    (catalogURL cat ++ ["_search"])
    (mwhen scroll $ [("scroll", Just scrollTime)])
    (JE.pairs $
       (mwhen (queryOffset > 0) $ "from" J..= queryOffset)
    <> (mwhen (queryLimit  > 0 || not scroll) $ "size" J..= queryLimit)
    <> "sort" `JE.pair` JE.list (\(f, a) -> JE.pairs (fieldName f J..= if a then "asc" else "desc" :: String)) (querySort ++ [(def{ fieldName = "_doc" },True)])
    <> (case store of
      ESStoreSource -> "_source"
      ESStoreValues -> "docvalue_fields"
      ESStoreStore -> "stored_fields") J..= map fieldName queryFields
    <> "query" .=* (if querySample < 1
      then \q -> ("function_score" .=* ("query" .=* q
        <> "random_score" .=* foldMap (\s -> "seed" J..= s <> "field" J..= ("_seq_no" :: String)) querySeed
        <> "boost_mode" J..= ("replace" :: String)
        <> "min_score" J..= (1 - querySample)))
      else id) filts
    <> "aggs" .=*
      (foldMap
        (\f -> fieldName f .=* ((if isTermsField f then "terms" else "stats") .=* field f))
        queryAggs
      <> histogram hists))
  where
  amend :: [FieldValue] -> J.Value -> J.Value
  amend h@(_:_) (J.Object o) = J.Object $ HM.insert "histsize" (J.toJSON $ map fieldType h) o
  amend _ j = j
  filts = "bool" .=* ("filter" `JE.pair` JE.list (\f -> JE.pairs $ unTypeValue (term f) $ fieldType f) queryFilter)
  term f (FilterEQ v) = "term" .=* (fieldName f J..= v)
  term f (FilterRange l u) = "range" .=* (fieldName f .=* (bound "gte" l <> bound "lte" u)) where
    bound t = foldMap (t J..=)
  histogram :: [FieldValue] -> J.Series
  histogram [] = mempty
  histogram (f:hl) = "hist" .=* (
    "histogram" .=* (field f
      <> "interval" J..= fieldType f
      <> "min_doc_count" J..= J.Number 1)
    <> histogram' hl)
  histogram' [] = mempty
  histogram' hl = "aggs" .=* histogram hl
  histsize :: (FieldSub Filter Proxy, Word) -> Maybe FieldValue
  histsize (f, n) = f' <$> case fieldType f of
    Double    (FilterRange (Just l) (Just u)) -> return $ Double    $ checksize $ (u - l) / realToFrac n
    Float     (FilterRange (Just l) (Just u)) -> return $ Float     $ checksize $ (u - l) / realToFrac n
    HalfFloat (FilterRange (Just l) (Just u)) -> return $ HalfFloat $ checksize $ (u - l) / realToFrac n
    Long      (FilterRange (Just l) (Just u)) -> return $ Long      $ checksize $ (u - l + n' - 1) `div` n' where n' = fromIntegral n
    Integer   (FilterRange (Just l) (Just u)) -> return $ Integer   $ checksize $ (u - l + n' - 1) `div` n' where n' = fromIntegral n
    Short     (FilterRange (Just l) (Just u)) -> return $ Short     $ checksize $ (u - l + n' - 1) `div` n' where n' = fromIntegral n
    Byte      (FilterRange (Just l) (Just u)) -> return $ Byte      $ checksize $ (u - l + n' - 1) `div` n' where n' = fromIntegral n
    _ -> fail "invalid hist"
    where
    f' s = f{ fieldSub = Proxy, fieldType = s }
  checksize z
    | z <= 0 = Identity 1
    | otherwise = Identity z
  fillhistbnd j = do
    aggs <- j J..: "aggregations"
    forM histunks $ \(f, n) -> do
      let val a = traverseTypeValue (maybe (fail "bnd value") return) . parseTypeJSONValue (fieldType f)
            =<< (J..: "value") =<< aggs J..: a
      lb <- val ("0" <> fieldName f)
      ub <- val ("1" <> fieldName f)
      return (liftFilterValue f $ FilterRange (Just lb) (Just ub), n)
  histunks :: [(Field, Word)]
  (histunks, histbnds) = partitionEithers $ map (\h@(f, n) -> 
      maybe (Left h) (Right . (, n)) $ find (\q -> fieldName f == fieldName q && unTypeValue isbnd (fieldType q)) queryFilter)
    queryHist
  isbnd (FilterRange (Just _) (Just _)) = True
  isbnd _ = False
  field = ("field" J..=) . fieldName

queryIndex :: Catalog -> Query -> M J.Value
queryIndex = queryIndexScroll False

scrollSearch :: T.Text -> M J.Value
scrollSearch sid = elasticSearch GET ["_search", "scroll"] [] $ JE.pairs $
     "scroll" J..= J.String scrollTime
  <> "scroll_id" J..= sid

queryBulk :: Catalog -> Query -> M (IO (Word, V.Vector [J.Value]))
queryBulk cat@Catalog{ catalogStore = CatalogES{ catalogStoreField = store } } query@Query{..} = do
  glob <- ask
  sidv <- liftIO $ newIORef Nothing
  return $ do
    sid <- readIORef sidv
    res <- runGlobal glob $ maybe
      (queryIndexScroll True cat query)
      scrollSearch
      sid
    (sid', rows) <- either fail return $ J.parseEither parse res
    writeIORef sidv $ Just sid'
    return rows
  where
  parse = J.withObject "query" $ \q -> (,)
    <$> q J..: "_scroll_id"
    <*> parseJSONField "hits" (J.withObject "hits" $ \hits -> (,)
      <$> hits J..: "total"
      <*> parseJSONField "hits" (J.withArray "hits" $
        V.mapM $ J.withObject "hit" $ case store of
          ESStoreSource ->
            parseJSONField "_source" $ J.withObject "source" $ \d ->
              return $ map (\f -> HM.lookupDefault J.Null (fieldName f) d) queryFields
          _ ->
            parseJSONField "fields" $ J.withObject "fields" $ \d ->
              return $ map (\f -> unsingletonJSON $ HM.lookupDefault J.Null (fieldName f) d) queryFields) hits)
      q

createBulk :: Catalog -> [(String, J.Series)] -> M ()
createBulk cat@Catalog{ catalogStore = ~CatalogES{} } docs = do
  r <- elasticSearch POST (catalogURL cat ++ ["_bulk"]) [] body
  -- TODO: ignore 409
  unless (HM.lookup "errors" (r :: J.Object) == Just (J.Bool False)) $ fail $ "createBulk: " ++ BSLC.unpack (J.encode r)
  where
  body = foldMap doc docs
  doc (i, d) = J.fromEncoding (J.pairs $ "create" .=* ("_id" J..= i))
    <> nl <> J.fromEncoding (J.pairs d) <> nl
  nl = B.char7 '\n'

flushIndex :: Catalog -> M J.Value
flushIndex Catalog{ catalogStore = ~CatalogES{ catalogIndex = idxn } } =
  elasticSearch POST ([T.unpack idxn, "_flush"]) [] EmptyJSON

newtype ESCount = ESCount{ esCount :: Word } deriving (Show)
instance J.FromJSON ESCount where
  parseJSON = fmap ESCount <$> J.withObject "count" (parseJSONField "count" $ \case
    J.String t -> either fail return $ readEither (T.unpack t)
    v -> J.parseJSON v)

countIndex :: Catalog -> M Word
countIndex Catalog{ catalogStore = ~CatalogES{ catalogIndex = idxn } } =
  sum . map esCount <$> elasticSearch GET (["_cat", "count", T.unpack idxn]) [] EmptyJSON

closeIndex :: Catalog -> M J.Value
closeIndex Catalog{ catalogStore = ~CatalogES{ catalogIndex = idxn } } =
  elasticSearch PUT ([T.unpack idxn, "_settings"]) [] $ JE.pairs $
    "index" .=* ("blocks" .=* ("read_only" J..= True))
