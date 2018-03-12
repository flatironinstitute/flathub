{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module ES
  ( module ES.Types
  , initServer
  , IndexSettings(..)
  , createIndex
  , getIndices
  , Query(..)
  , queryIndex
  , scrollSearch
  ) where

import           Control.Monad ((<=<))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J (parseEither)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Default (Default(def))
import qualified Data.HashMap.Strict as HM
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Header (hAccept, hContentType)
import           Network.HTTP.Types.Method (StdMethod(GET, PUT), renderStdMethod)
import qualified Network.HTTP.Types.URI as HTTP (Query)
import qualified Network.URI as URI
import qualified Waimwork.Config as C

import JSON
import ES.Types
import Schema
import Global

initServer :: C.Config -> IO Server
initServer conf = Server
  <$> HTTP.parseUrlThrow (conf C.! "server")

elasticSearch :: J.FromJSON r => StdMethod -> [String] -> HTTP.Query -> Maybe J.Encoding -> M r
elasticSearch meth url query body = do
  glob <- ask
  let req = serverRequest $ globalES glob
      req' = maybe id setBody body $
        HTTP.setQueryString query req
        { HTTP.method = renderStdMethod meth
        , HTTP.path = HTTP.path req <> BS.intercalate "/" (map (BSC.pack . URI.escapeURIString URI.isUnescapedInURIComponent) url)
        , HTTP.requestHeaders = (hAccept, "application/json") : HTTP.requestHeaders req
        }
  liftIO $ do
    -- print $ HTTP.path req'
    -- print $ JE.encodingToLazyByteString <$> body
    either fail return . (J.parseEither J.parseJSON <=< AP.eitherResult)
      =<< HTTP.withResponse req' (globalHTTP glob) parse
  where
  setBody bod req = req
    { HTTP.requestHeaders = (hContentType, "application/json") : HTTP.requestHeaders req
    , HTTP.requestBody = HTTP.RequestBodyLBS $ JE.encodingToLazyByteString bod
    }
  parse r = AP.parseWith (HTTP.responseBody r) J.json BS.empty

data IndexSettings = IndexSettings
  { indexNumberOfShards
  , indexNumberOfReplicas
  , indexNumberOfRoutingShards :: Word
  }

instance Default IndexSettings where
  def = IndexSettings
    { indexNumberOfShards = 8
    , indexNumberOfReplicas = 1
    , indexNumberOfRoutingShards = 8
    }

instance J.ToJSON IndexSettings where
  toJSON IndexSettings{..} = J.object
    [ "number_of_shards" J..= indexNumberOfShards
    , "number_of_replicas" J..= indexNumberOfReplicas
    , "number_of_routing_shards" J..= indexNumberOfRoutingShards
    ]
  toEncoding IndexSettings{..} = J.pairs
    (  "number_of_shards" J..= indexNumberOfShards
    <> "number_of_replicas" J..= indexNumberOfReplicas
    <> "number_of_routing_shards" J..= indexNumberOfRoutingShards
    )

createIndex :: IndexSettings -> Catalog -> M J.Value
createIndex sets cat = elasticSearch PUT [T.unpack $ catalogIndex cat] [] $ Just $ JE.pairs $
     "settings" .=*
    (  "index" J..= sets)
  <> "mappings" .=*
    (  catalogMapping cat .=*
      (  "dynamic" J..= J.String "strict"
      <> "properties" J..= HM.map fieldType (catalogFieldMap cat)))

getIndices :: [String] -> M J.Value
getIndices idx = elasticSearch GET [if null idx then "_all" else intercalate "," idx] [] Nothing

data Query = Query
  { queryOffset :: Word
  , queryLimit :: Word
  , querySort :: [(T.Text, Bool)]
  , queryFields :: [T.Text]
  , queryFilter :: [(T.Text, BS.ByteString, Maybe BS.ByteString)]
  , queryAggs :: [T.Text]
  , queryHist :: Maybe T.Text
  , queryScroll :: Bool
  }

instance Monoid Query where
  mempty = Query
    { queryOffset = 0
    , queryLimit  = 0
    , querySort   = []
    , queryFields = []
    , queryFilter = []
    , queryAggs   = []
    , queryHist   = Nothing
    , queryScroll = False
    }
  mappend q1 q2 = Query
    { queryOffset = queryOffset q1 +     queryOffset q2
    , queryLimit  = queryLimit  q1 `max` queryLimit  q2
    , querySort   = querySort   q1 <>    querySort   q2
    , queryFields = queryFields q1 <>    queryFields q2
    , queryFilter = queryFilter q1 <>    queryFilter q2
    , queryAggs   = queryAggs   q1 <>    queryAggs   q2
    , queryHist   = queryHist   q1 <>    queryHist   q2
    , queryScroll = queryScroll q1 ||    queryScroll q2
    }

scrollTime :: IsString s => s
scrollTime = "10s"

queryIndex :: Catalog -> Query -> M J.Value
queryIndex cat Query{..} =
  elasticSearch GET
    [T.unpack $ catalogIndex cat, T.unpack $ catalogMapping cat, "_search"]
    (mif queryScroll $ [("scroll", Just scrollTime)])
    $ Just $ JE.pairs $
       (mif (queryOffset > 0) $ "from" J..= queryOffset)
    <> (mif (queryLimit  > 0) $ "size" J..= queryLimit)
    <> "sort" `JE.pair` JE.list (\(f, a) -> JE.pairs (f J..= if a then "asc" else "desc" :: String)) (querySort ++ [("_doc",True)])
    <> (mif (not (null queryFields)) $
       "_source" J..= queryFields)
    <> "query" .=*
      ("bool" .=*
        ("filter" `JE.pair` JE.list (JE.pairs . term) queryFilter))
    <> (mif (not (null queryAggs)) $
       "aggs" .=* (foldMap
      (\f -> f .=* (agg (fieldType <$> HM.lookup f (catalogFieldMap cat)) .=* ("field" J..= f)))
      queryAggs))
  where
  mif True v = v
  mif False _ = mempty
  term (f, a, Nothing) = "term" .=* (f `JE.pair` bsc a)
  term (f, a, Just b) = "range" .=* (f .=*
    (bound "gte" a <> bound "lte" b))
  bound t a
    | BS.null a = mempty
    | otherwise = t `JE.pair` bsc a
  agg (Just Text) = "terms"
  agg (Just Keyword) = "terms"
  agg _ = "stats"
  bsc = JE.string . BSC.unpack

scrollSearch :: T.Text -> M J.Value
scrollSearch sid = elasticSearch GET ["_search", "scroll"] [] $ Just $ JE.pairs $
     "scroll" J..= J.String scrollTime
  <> "scroll_id" J..= sid
