{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PG
  ( createTable
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import qualified Data.ByteString.Builder as BSB
import           Data.Monoid ((<>))
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Database.PostgreSQL.Typed.Types (pgDQuote)
import qualified Waimwork.Database.PostgreSQL as PG

import Schema
import Global

withPG :: (PG.DBConn -> IO a) -> M a
withPG f = do
  db <- asks globalPG
  liftIO $ PG.withDB db f

pgIdent :: T.Text -> BSB.Builder
pgIdent = pgDQuote ['A'..'Z'] . TE.encodeUtf8

pgType :: IsString s => Type -> s
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
  PG.pgExecute_ db $ BSB.toLazyByteString $ "CREATE TABLE " <> tab <> " (_id bigserial primary key " <> foldMap col fields <> ");" <> foldMap idx fields
  where
  tab = pgIdent tabn
  fields = expandFields $ catalogFields cat
  col Field{..} = "," <> pgIdent fieldName <> " " <> pgType fieldType
  idx Field{..} = "CREATE INDEX ON " <> tab <> " (" <> pgIdent fieldName <> ");"
createTable _ = return ()
