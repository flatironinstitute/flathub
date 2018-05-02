{-# LANGUAGE FlexibleInstances #-}
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
  ) where

import           Control.Monad ((<=<), forM_, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J (Parser, parseEither)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Header (hAccept, hContentType)
import           Network.HTTP.Types.Method (StdMethod(GET, PUT, POST), renderStdMethod)
import qualified Network.HTTP.Types.URI as HTTP (Query)
import qualified Network.URI as URI
import qualified Waimwork.Config as C

import Monoid
import JSON
import Schema
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

elasticSearch :: (Body b, J.FromJSON r) => StdMethod -> [String] -> HTTP.Query -> b -> M r
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
    either fail return . (J.parseEither J.parseJSON <=< AP.eitherResult)
      =<< HTTP.withResponse req' (globalHTTP glob) parse
  where
  parse r = AP.parseWith (HTTP.responseBody r) J.json BS.empty

catalogURL :: Catalog -> [String]
catalogURL Catalog{ catalogStore = CatalogES{ catalogIndex = idxn, catalogMapping = mapn } } =
  [T.unpack idxn, T.unpack mapn]
catalogURL _ = error "catalogURL: non-ES catalog"

createIndex :: Catalog -> M J.Value
createIndex cat@Catalog{ catalogStore = CatalogES idxn mapn sets } = elasticSearch PUT [T.unpack idxn] [] $ JE.pairs $
     "settings" J..= sets
  <> "mappings" .=*
    (  mapn .=*
      (  "dynamic" J..= J.String "strict"
      <> "properties" J..= HM.map field (catalogFieldMap cat)))
  where
  field f = J.object ["type" J..= fieldType f]
createIndex _ = return J.Null

checkIndices :: M ()
checkIndices = do
  cats <- asks $ filter ises . HM.elems . globalCatalogs
  indices <- elasticSearch GET [intercalate "," $ map catalogIndex' cats] [] ()
  either
    (fail . ("ES index mismatch: " ++))
    return
    $ J.parseEither (J.withObject "indices" $ forM_ cats . catalog) indices
  where
  ises Catalog{ catalogStore = CatalogES{} } = True
  ises _ = False
  catalogIndex' ~Catalog{ catalogStore = CatalogES{ catalogIndex = idxn} } = T.unpack idxn
  catalog is ~cat@Catalog{ catalogStore = CatalogES{ catalogIndex = idxn, catalogMapping = mapn } } = parseJSONField idxn (idx cat mapn) is
  idx :: Catalog -> T.Text -> J.Value -> J.Parser ()
  idx cat mapn = J.withObject "index" $ parseJSONField "mappings" $ J.withObject "mappings" $
    parseJSONField mapn (mapping $ catalogFields cat)
  mapping :: Fields -> J.Value -> J.Parser ()
  mapping fields = J.withObject "mapping" $ parseJSONField "properties" $ J.withObject "properties" $ \ps ->
    forM_ fields $ \field -> parseJSONField (fieldName field) (prop field) ps
  prop :: Field -> J.Value -> J.Parser ()
  prop field = J.withObject "property" $ \p -> do
    t <- p J..: "type"
    unless (t == fieldType field) $ fail $ "incorrect field type; should be " ++ show (fieldType field)

scrollTime :: IsString s => s
scrollTime = "10s"

queryIndexScroll :: Bool -> Catalog -> Query -> M J.Value
queryIndexScroll scroll cat@Catalog{ catalogStore = CatalogES{} } Query{..} =
  elasticSearch GET
    (catalogURL cat ++ ["_search"])
    (mwhen scroll $ [("scroll", Just scrollTime)])
    $ JE.pairs $
       (mwhen (queryOffset > 0) $ "from" J..= queryOffset)
    <> (mwhen (queryLimit  > 0 || not scroll) $ "size" J..= queryLimit)
    <> "sort" `JE.pair` JE.list (\(f, a) -> JE.pairs (f J..= if a then "asc" else "desc" :: String)) (querySort ++ [("_doc",True)])
    <> "_source" J..= queryFields
    <> "query" .=*
      ("bool" .=*
        ("filter" `JE.pair` JE.list (JE.pairs . term) queryFilter))
    <> "aggs" .=*
      (  foldMap
        (\f -> f .=* (agg (fieldType <$> HM.lookup f (catalogFieldMap cat)) .=* ("field" J..= f)))
        queryAggs
      <> foldMap (\(f, i) -> "hist" .=* (
        "histogram" .=* (
             "field" J..= f
          <> "interval" `JE.pair` bsc i)))
        queryHist)
  where
  term (f, a, Nothing) = "term" .=* (f `JE.pair` bsc a)
  term (f, a, Just b) = "range" .=* (f .=*
    (bound "gte" a <> bound "lte" b))
  bound t a
    | BS.null a = mempty
    | otherwise = t `JE.pair` bsc a
  agg (Just (Text _)) = "terms"
  agg (Just (Keyword _)) = "terms"
  agg _ = "stats"
  bsc = JE.string . BSC.unpack
queryIndexScroll _ _ _ = return J.Null

queryIndex :: Catalog -> Query -> M J.Value
queryIndex = queryIndexScroll False

scrollSearch :: T.Text -> M J.Value
scrollSearch sid = elasticSearch GET ["_search", "scroll"] [] $ JE.pairs $
     "scroll" J..= J.String scrollTime
  <> "scroll_id" J..= sid

queryBulk :: Catalog -> Query -> M (IO (V.Vector [J.Value]))
queryBulk cat query@Query{..} = do
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
    <*> parseJSONField "hits" (J.withObject "hits" $
      parseJSONField "hits" $ J.withArray "hits" $
        V.mapM $ J.withObject "hit" $
          parseJSONField "_source" $ J.withObject "source" $ \d ->
            return $ map (\f -> HM.lookupDefault J.Null f d) queryFields)
      q

createBulk :: Catalog -> [(String, J.Series)] -> M ()
createBulk cat@Catalog{ catalogStore = CatalogES{} } docs = do
  r <- elasticSearch POST (catalogURL cat ++ ["_bulk"]) [] body
  unless (HM.lookup "errors" (r :: J.Object) == Just (J.Bool False)) $ fail $ "createBulk: " ++ BSLC.unpack (J.encode r)
  where
  body = foldMap doc docs
  doc (i, d) = J.fromEncoding (J.pairs $ "create" .=* ("_id" J..= i))
    <> nl <> J.fromEncoding (J.pairs d) <> nl
  nl = B.char7 '\n'
createBulk _ _ = fail "createBulk: non-ES catalog"

flushIndex :: Catalog -> M ()
flushIndex cat@Catalog{ catalogStore = CatalogES{} } =
  elasticSearch POST (catalogURL cat ++ ["_flush"]) [] ()
flushIndex _ = return ()
