{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ES
  ( module ES.Types
  , initServer
  , getIndices
  , queryIndex
  ) where

import           Control.Monad ((<=<))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J (parseEither, emptyObject)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Header (hAccept, hContentType)
import           Network.HTTP.Types.Method (StdMethod(GET), renderStdMethod)
import qualified Network.URI as URI
import qualified Waimwork.Config as C

import ES.Types
import Global

initServer :: C.Config -> IO Server
initServer conf = Server
  <$> HTTP.parseRequest (conf C.! "server")

elasticSearch :: J.FromJSON r => StdMethod -> [String] -> Maybe J.Encoding -> M r
elasticSearch meth url body = do
  glob <- ask
  let req = serverRequest $ globalES glob
      req' =
        maybe id setBody body req
        { HTTP.method = renderStdMethod meth
        , HTTP.path = HTTP.path req <> BS.intercalate "/" (map (BSC.pack . URI.escapeURIString URI.isUnescapedInURIComponent) url)
        , HTTP.requestHeaders = (hAccept, "application/json") : HTTP.requestHeaders req
        }
  -- HTTP.setRequestIgnoreStatus
  liftIO $
    either fail return . (J.parseEither J.parseJSON <=< AP.eitherResult)
      =<< HTTP.withResponse req' (globalHTTP glob) parse
  where
  setBody bod req = req
    { HTTP.requestHeaders = (hContentType, "application/json") : HTTP.requestHeaders req
    , HTTP.requestBody = HTTP.RequestBodyLBS $ JE.encodingToLazyByteString bod
    }
  parse r = AP.parseWith (HTTP.responseBody r) J.json BS.empty

getIndices :: [String] -> M (KeyMap Index)
getIndices idx = elasticSearch GET [if null idx then "_all" else intercalate "," idx] Nothing

queryIndex :: String -> Index -> Query -> M J.Value
queryIndex idx maps q =
  clean <$> elasticSearch GET
    [idx, T.unpack $ mappingName maps, "_search"]
    (Just $ JE.pairs $
       "from" J..= queryOffset q
    <> "size" J..= queryLimit q
    <> "sort" `JE.pair` JE.list (\(f, a) -> JE.pairs (f J..= if a then "asc" else "desc" :: String)) (querySort q)
    <> (if null (queryFields q) then mempty else
       "_source" J..= queryFields q)
    <> "query" `JE.pair` JE.pairs
      ("bool" `JE.pair` JE.pairs
        ("filter" `JE.pair` JE.list (JE.pairs . term) (queryFilter q)))
    <> "aggs" `JE.pair` JE.pairs (foldMap
      (\f -> f `JE.pair` JE.pairs (agg (HM.lookup f $ indexMapping maps) `JE.pair` JE.pairs ("field" J..= f)))
      (queryAggs q)))
  where
  term (f, a, Nothing) = "term" `JE.pair` JE.pairs (f `JE.pair` bsc a)
  term (f, a, Just b) = "range" `JE.pair` JE.pairs (f `JE.pair` JE.pairs
    (bound "gte" a <> bound "lte" b))
  bound t a
    | BS.null a = mempty
    | otherwise = t `JE.pair` bsc a
  agg (Just (FieldInfo Text)) = "terms"
  agg (Just (FieldInfo Keyword)) = "terms"
  agg _ = "stats"
  bsc = JE.string . BSC.unpack
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
