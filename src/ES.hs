{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module ES
  ( initServer
  , HTTP.Request
  , httpJSON
  , httpStream
  , searchCatalogRequest
  , searchCatalog
  , createIndex
  , checkIndices
  , storedFieldsArgs
  , storedFields
  , maxResultWindow
  , queryIndex
  , queryBulk
  , createBulk
  , flushIndex
  , countIndex
  , closeIndex
  , openIndex
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad (forM, forM_, when, unless, (<=<))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ask, asks)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J (Parser, parseEither, parseMaybe, typeMismatch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Conduit as C
import           Data.Either (partitionEithers)
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.List (find, partition)
import           Data.Maybe (mapMaybe, fromMaybe, maybeToList, isJust)
import           Data.Proxy (Proxy)
import           Data.String (IsString)
import qualified Data.Text as T
import           Data.Typeable (cast)
import qualified Data.Vector as V
import qualified Network.HTTP.Client.Conduit as HTTP hiding (httpSource)
import qualified Network.HTTP.Simple as HTTP
import           Network.HTTP.Types.Header (hAcceptEncoding, hAccept, hContentType)
import           Network.HTTP.Types.Method (StdMethod(GET, PUT, POST), renderStdMethod)
import qualified Network.URI as URI
import           Text.Read (readEither)
import qualified Waimwork.Config as C
import Debug.Trace

import Monoid
import JSON
import Type
import Field
import Catalog
import Error
import Global

debug :: Bool
debug = False

initServer :: C.Config -> IO HTTP.Request
initServer conf = do
  req <- HTTP.parseRequestThrow (conf C.! "server")
  return req
    { HTTP.requestHeaders =
        [ (hAcceptEncoding, " ") -- disable gzip
        , (hAccept, "application/json")
        ]
    }

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

elasticRequest :: (MonadGlobal m, Body b) => StdMethod -> [String] -> HTTP.Query -> b -> m HTTP.Request
elasticRequest meth url query body = do
  req <- asks globalES
  when debug $ do
    traceShowM $ HTTP.path req
    traceM $ case bodyRequest body of
      HTTP.RequestBodyBS b -> BSC.unpack b
      HTTP.RequestBodyLBS b -> BSLC.unpack b
      _ -> "???"
  return $ HTTP.setQueryString query req
    { HTTP.method = renderStdMethod meth
    , HTTP.path = HTTP.path req <> BS.intercalate "/" (map (BSC.pack . URI.escapeURIString URI.isUnescapedInURIComponent) url)
    , HTTP.requestHeaders = maybe id ((:) . (,) hContentType) (bodyContentType body)
        $ HTTP.requestHeaders req
    , HTTP.requestBody = bodyRequest body
    }

httpJSON :: (MonadIO m, MonadErr m) => (J.Value -> J.Parser r) -> HTTP.Request -> m r
httpJSON pares req = do
  j <- liftIO $ HTTP.getResponseBody <$> HTTP.httpJSON req
  when debug $ traceM $ BSLC.unpack $ J.encode j
  either raise500 return $ J.parseEither pares j

httpPrint :: HTTP.Request -> M ()
httpPrint req = liftIO $ BSC.putStrLn . HTTP.getResponseBody =<< HTTP.httpBS req

httpStream :: (MonadResource m, MonadIO m) => HTTP.Request -> C.ConduitM i BS.ByteString m ()
httpStream req = HTTP.httpSource req HTTP.getResponseBody

catalogURL :: Catalog -> [String]
catalogURL Catalog{ catalogIndex = idxn } =
  [T.unpack idxn]

searchCatalogRequest :: (MonadGlobal m, Body b) => Catalog -> HTTP.Query -> b -> m HTTP.Request
searchCatalogRequest cat q b = elasticRequest GET (catalogURL cat ++ ["_search"]) q b

searchCatalog :: (MonadMIO m, Body b) => Catalog -> HTTP.Query -> (J.Value -> J.Parser r) -> b -> m r
searchCatalog cat q p b = httpJSON p =<< searchCatalogRequest cat q b

defaultSettings :: Catalog -> J.Object
defaultSettings cat = HM.fromList
  [ "index" J..= J.object (
    [ "number_of_shards" J..= (clusterSize * min 100 (maybe 2 (succ . (`div` (clusterSize * docsPerShard))) (catalogCount cat)))
    , "number_of_replicas" J..= J.Number 2
    , "refresh_interval" J..= J.Number (-1)
    , "max_docvalue_fields_search" J..= (8 + length (catalogFields cat))
    ] ++ case catalogSort cat of
      [] -> []
      s ->
        [ "sort.field" J..= map J.String s
        , "sort.order" J..= map (const $ J.String "asc") s
        ]
    ++ maybeToList (("default_pipeline" J..= catalogIndex cat) <$ catalogIngestPipeline cat))
  ] where
  -- elastic search cluster size to optimize for (should really be determined dynamicaly)
  clusterSize = 4
  -- target number of docs per shard
  docsPerShard = 100000000

arrayMeta :: Bool -> Maybe String
arrayMeta False = Nothing
arrayMeta True = Just "1"

createIndex :: Catalog -> M ()
createIndex cat@Catalog{..} = do
  mapM_ (\src ->
    httpPrint =<< elasticRequest PUT ["_ingest","pipeline",T.unpack catalogIndex] [] (JE.pairs
      $ "processors" .=*
        ("script" .=*
          ("source" J..= src))
      )) catalogIngestPipeline
  httpPrint =<< elasticRequest PUT [T.unpack catalogIndex] [] (JE.pairs
    $  "settings" J..= mergeJSONObject catalogIndexSettings (defaultSettings cat)
    <> "mappings" .=*
      (  "dynamic" J..= J.String "strict"
      <> "_source" .=* ("enabled" J..= False)
      <> "properties" .=* HM.foldMapWithKey field catalogFieldMap))
  where
  field n f = n .=*
    (  "type" J..= t
    <> "store" J..= and (fieldStore f)
    <> "index" J..= not (or $ fieldStore f)
    <> foldMap (("meta" .=*) . ("array" J..=)) (arrayMeta a))
    where (a, t) = unArrayType (fieldType f)

checkIndices :: M (HM.HashMap Simulation String)
checkIndices = do
  isdev <- asks globalDevMode
  indices <- httpJSON J.parseJSON =<< elasticRequest GET ["*"] [] ()
  HM.mapMaybe (\cat -> either Just (const Nothing) $ J.parseEither (catalog isdev cat) indices)
    <$> asks (catalogMap . globalCatalogs)
  where
  catalog isdev cat@Catalog{ catalogIndex = idxn } =
    parseJSONField idxn (idx isdev cat)
  idx :: Bool -> Catalog -> J.Value -> J.Parser ()
  idx isdev cat = J.withObject "index" $ \i -> do
    sets <- i J..: "settings" >>= (J..: "index")
    ro <- sets J..:? "blocks" >>= maybe (return Nothing) (J..:? "read_only") >>= maybe (return False) boolish
    unless (isdev || ro || not (catalogVisible cat)) $ fail "open (not read_only)"
    parseJSONField "mappings" (mapping $ catalogFields cat) i
  mapping :: Fields -> J.Value -> J.Parser ()
  mapping fields = J.withObject "mapping" $ parseJSONField "properties" $ J.withObject "properties" $ \ps ->
    forM_ fields $ \field -> parseJSONField (fieldName field) (prop field) ps
  prop :: Field -> J.Value -> J.Parser ()
  prop field = J.withObject "property" $ \p -> do
    let (fa, ft) = unArrayType (fieldType field)
    t <- p J..: "type"
    m <- p J..:? "meta" J..!= HM.empty
    a <- m J..:? "array"
    unless (t == ft && a == arrayMeta fa) $
      fail $ "incorrect field type; should be " ++ show (fieldType field)
  boolish :: J.Value -> J.Parser Bool
  boolish (J.Bool b) = return b
  boolish (J.String "true") = return True
  boolish (J.String "false") = return False
  boolish v = J.typeMismatch "bool" v

storedFieldsArgs :: [Field] -> J.Series
storedFieldsArgs fields =
  req "stored_fields" store <>
  req "docvalue_fields" docvalue
  where
  req _ [] = mempty
  req n l = n J..= map fieldName l
  (store, docvalue) = partition (and . fieldStore) fields

storedFields :: J.Object -> J.Object
storedFields o = maybe id (HM.insert "_id") (HM.lookup "_id" o) $
  foldMap obj (HM.lookup "fields" o)
  where
  obj (J.Object x) = x
  obj _ = error "stored fields" -- HM.empty

scrollTime :: IsString s => s
scrollTime = "60s"

maxResultWindow :: Word
maxResultWindow = 10000

data HistogramInterval a
  = HistogramInterval
    { histogramInterval :: a
    , _histogramRanges :: [(a, a)] -- (log, exp)
    }

queryIndexScroll :: Bool -> Catalog -> Query -> M J.Value
queryIndexScroll scroll cat Query{..} = do
  hists <- gethists
  amend hists <$> searchCatalog cat
    (mwhen scroll $ [("scroll", Just scrollTime)])
    J.parseJSON
    (JE.pairs $
       (mwhen (queryOffset > 0) $ "from" J..= queryOffset)
    <> ("size" J..= if scroll && queryLimit == 0 then maxResultWindow else queryLimit)
    <> "sort" `JE.pair` JE.list (\(f, a) -> JE.pairs (fieldName f J..= if a then "asc" else "desc" :: String)) (querySort ++ [(docField,True)])
    <> storedFieldsArgs queryFields
    <> "query" .=* (if querySample < 1
      then \q -> ("function_score" .=* ("query" .=* q
        <> "random_score" .=* foldMap (\s -> "seed" J..= s <> "field" J..= ("_seq_no" :: String)) querySeed
        <> "boost_mode" J..= ("replace" :: String)
        <> "min_score" J..= (1 - querySample)))
      else id) filts
    <> "track_total_hits" J..= J.Bool True
    <> aggs hists queryAggs)
  where
  -- find all histogram fields and divide them into those with range queries and those without
  -- if a field appears in multiple histograms, we use the same (arbitrary) size for all
  -- this could be easily fixed, but would require more complex histsize results
  (histunks, histbnds) = partitionEithers $ map (\h@(f, t, n) ->
      maybe (Left h) (Right . (, t, n)) $
        find (\q -> fieldName f == fieldName q && unTypeValue isbnd (fieldType q)) queryFilter)
    $ foldMap histFields queryAggs
  isbnd (FilterRange (Just _) (Just _)) = True
  isbnd _ = False
  histFields (QueryHist f n t l) = (f, t, n) : foldMap histFields l
  histFields _ = []

  -- pre-query ranges of histogram fields without bounds if necesary
  gethists = HM.fromList . mapMaybe histsize . (histbnds ++) <$> if null histunks then return [] else
    fold . J.parseMaybe fillhistbnd <$> searchCatalog cat
      [] J.parseJSON
      (JE.pairs
      $  "size" J..= J.Number 0
      <> "query" .=* filts
      <> "aggs" .=* foldMap (\(f, _, _) ->
           ("0" <> fieldName f) .=* ("min" .=* field f)
        <> ("1" <> fieldName f) .=* ("max" .=* field f))
        histunks)
  -- resolve bounds in pre-query result
  fillhistbnd j = do
    jaggs <- j J..: "aggregations"
    forM histunks $ \(f, t, n) -> do
      let val a = traverseValue (maybe (fail "bnd value") return) . parseTypeJSONValue (fieldType f)
            =<< (J..: "value") =<< jaggs J..: a
      lb <- val ("0" <> fieldName f)
      ub <- val ("1" <> fieldName f)
      return (liftFilterValue f $ FilterRange (Just lb) (Just ub), t, n)
  -- calculate bucket size from range and count
  histsize :: (FieldSub Filter Proxy, Bool, Word) -> Maybe (T.Text, TypeValue HistogramInterval)
  histsize (f, t, n) = case fieldType f of
    Double    (FilterRange (Just l) (Just u)) -> q Double    l u
    Float     (FilterRange (Just l) (Just u)) -> q Float     l u
    HalfFloat (FilterRange (Just l) (Just u)) -> q HalfFloat l u
    Long      (FilterRange (Just l) (Just u)) -> i Long      l u
    Integer   (FilterRange (Just l) (Just u)) -> i Integer   l u
    Short     (FilterRange (Just l) (Just u)) -> i Short     l u
    Byte      (FilterRange (Just l) (Just u)) -> i Byte      l u
    _ -> fail "invalid hist"
    where
    r :: (Num t, Ord t) => (forall f . f t -> TypeValue f) -> (t -> [(t, t)]) -> t -> Maybe (T.Text, TypeValue HistogramInterval)
    r c l s = return (fieldName f, c $ HistogramInterval s' (l s')) where
      s' | s > 0 = s
         | otherwise = 1
    q :: (Floating t, Ord t, Enum t) => (forall f . f t -> TypeValue f) -> t -> t -> Maybe (T.Text, TypeValue HistogramInterval)
    q c l u
      | t && l > 0 = r c (\s -> map (id &&& exp) $ take (fromIntegral $ succ n) $ enumFromThenTo ll (ll + s) lu) $ (lu - ll) / realToFrac n
      | otherwise = r c (const []) $ (u - l) / realToFrac n
      where
      ll = log l
      lu = log u
    i :: Integral t => (forall f . f t -> TypeValue f) -> t -> t -> Maybe (T.Text, TypeValue HistogramInterval)
    i c l u
      | t = fail "log hist on int"
      | otherwise = r c (const []) $ (u - l + n' - 1) `div` n' where n' = fromIntegral n

  -- add hist field sizes (widths) to final result
  amend :: HM.HashMap T.Text (TypeValue HistogramInterval) -> J.Value -> J.Value
  amend h (J.Object o) | not (HM.null h') = J.Object $ HM.insert "histsize" (J.toJSON h') o where
    h' = HM.map (fmapTypeValue1 histogramInterval) h
  amend _ j = j

  filts = "bool" .=* ("filter" `JE.pair` JE.list (\f -> JE.pairs $ unTypeValue (term f) $ fieldType f) queryFilter)
  term f (FilterEQ v)
    | fieldWildcard f && any (T.any ('*' ==)) (cast v) = "wildcard" .=* (fieldName f J..= v)
    | otherwise = "term" .=* (fieldName f J..= v)
  term f (FilterRange l u) = "range" .=* (fieldName f .=* (bound "gte" l <> bound "lte" u)) where
    bound t = foldMap (t J..=)
  agg :: HM.HashMap T.Text (TypeValue HistogramInterval) -> QueryAgg -> J.Series
  agg _ (QueryStats f) = fieldName f .=* (if fieldTerms f || typeIsString (fieldType f)
    then "terms" .=* (field f <> "size" J..= (if fieldTerms f then 32 else 4 :: Int))
    else "stats" .=* field f)
  agg _ (QueryPercentiles f p) = "pct" .=* ("percentiles" .=* (field f <> "percents" J..= p))
  agg h (QueryHist f _ _ a)
    | Just v <- HM.lookup (fieldName f) h = "hist" .=*
      (unTypeValue (hist f) v <> aggs h a)
    | otherwise = mempty
  aggs _ [] = mempty
  aggs h a = "aggs" .=* foldMap (agg h) a
  hist f (HistogramInterval i []) = "histogram" .=* (field f
    <> "interval" J..= i
    <> "min_doc_count" J..= J.Number 1)
  hist f (HistogramInterval _ l) = "range" .=* (field f
    <> "ranges" `JE.pair` JE.list id (range l))
  range ((lx,x):r@((_,y):_)) = JE.pairs ("from" J..= x <> "to" J..= y <> "key" J..= show lx) : range r
  range _ = []

  field = ("field" J..=) . fieldName

queryIndex :: Catalog -> Query -> M J.Value
queryIndex = queryIndexScroll False

scrollSearch :: T.Text -> M J.Value
scrollSearch sid = httpJSON return =<< elasticRequest GET ["_search", "scroll"] [] (JE.pairs
  $  "scroll" J..= J.String scrollTime
  <> "scroll_id" J..= sid)

queryBulk :: Catalog -> Query -> M (IO (Word, V.Vector J.Object))
queryBulk cat query@Query{..} = do
  unless (queryOffset == 0 && null queryAggs) $
    raise400 "offset,aggs not supported for download"
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
      <$> (hits J..: "total" >>= (J..: "value"))
      <*> parseJSONField "hits" (J.withArray "hits" $
        V.mapM (J.withObject "hit" (return . row)))
      hits)
    q
  row = HM.map unsingletonJSON . storedFields

createBulk :: Foldable f => Catalog -> f (String, J.Series) -> M ()
createBulk cat docs = do
  conf <- asks globalConfig
  let act = fromMaybe "create" $ conf C.! "ingest_action"
      body = foldMap doc docs
      doc (i, d) = J.fromEncoding (J.pairs $ act .=* ("_id" J..= i))
        <> nl <> J.fromEncoding (J.pairs d) <> nl
  r <- httpJSON J.parseJSON =<< elasticRequest POST (catalogURL cat ++ ["_bulk"]) [] body
  -- TODO: ignore 409?
  unless (getelem "errors" r == Just (J.Bool False)) $ do
    liftIO $ V.mapM_ (BSLC.putStrLn . J.encode) (foldMap filterr $ getelem "items" r)
    raise500 "createBulk"
  where
  nl = B.char7 '\n'
  filterr (J.Array v) = V.filter (isJust . (getelem "error" <=< getelem "create")) v
  filterr j = V.singleton j
  getelem k (J.Object o) = HM.lookup k o
  getelem _ _ = Nothing

flushIndex :: Catalog -> M ()
flushIndex Catalog{ catalogIndex = idxn } = do
  httpPrint =<< elasticRequest POST ([T.unpack idxn, "_refresh"]) [] ()
  httpPrint =<< elasticRequest POST ([T.unpack idxn, "_flush"]) [] ()

countIndex :: Catalog -> M Word
countIndex Catalog{ catalogIndex = idxn } =
  httpJSON (fmap sum . parseCounts) =<< elasticRequest GET (["_cat", "count", T.unpack idxn]) [] EmptyJSON
  where
  parseCounts = J.withArray "counts" $ mapM $ J.withObject "count" (parseJSONField "count" $ \case
    J.String t -> either fail return $ readEither (T.unpack t)
    v -> J.parseJSON v)

blockIndex :: Bool -> Catalog -> M ()
blockIndex ro Catalog{ catalogIndex = idxn } =
  httpPrint =<< elasticRequest PUT ([T.unpack idxn, "_settings"]) [] (JE.pairs
    $ "index" .=* ("blocks" .=* ("read_only" J..= ro)))

closeIndex :: Catalog -> M ()
closeIndex = blockIndex True

openIndex :: Catalog -> M ()
openIndex = blockIndex False
