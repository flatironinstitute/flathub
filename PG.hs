{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PG
  ( createTable
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import qualified Waimwork.Database.PostgreSQL as PG

import Schema
import Global

withPG :: (PG.DBConn -> IO a) -> M a
withPG f = do
  db <- asks globalPG
  liftIO $ PG.withDB db f

pgType :: Type -> BS.ByteString
pgType Text = "text"
pgType Keyword = "text"
pgType Long = "bigint"
pgType Integer = "integer"
pgType Short = "smallint"
pgType Byte = "smallint" -- XXX
pgType Double = "double precision"
pgType Float = "real"
pgType HalfFloat = "real" -- XXX
pgType Date = "timestamp" -- tz
pgType Boolean = "boolean"
pgType Binary = "bytea"

createTable :: Catalog -> M ()
createTable cat@Catalog{ catalogStore = CatalogPG tabn } = withPG $ \db -> do
  PG.pgExecute_ db $ BSL.fromChunks $ ["CREATE TABLE ", tabn, " (_id bigserial primary key "] ++ foldMap col fields ++ ");" : foldMap idx fields
  where
  fields = expandFields $ catalogFields cat
  col Field{..} = [",", TE.encodeUtf8 fieldName, " ", pgType fieldType]
  idx Field{..} = ["CREATE INDEX ON ", tabn, " (", TE.encodeUtf8 fieldName, ");"]
createTable _ = return ()
