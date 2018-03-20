{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module PG
  ( createTable
  , checkTables
  , queryTable
  ) where

import           Control.Arrow (first)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.ByteString.Builder as BSB
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Database.PostgreSQL.Typed (pgSQL)
import           Database.PostgreSQL.Typed.Dynamic (PGRep, pgDecodeRep, pgLiteralRep)
import           Database.PostgreSQL.Typed.Protocol (pgSimpleQuery)
import           Database.PostgreSQL.Typed.Types (PGValue(..), pgDQuote)
import qualified Waimwork.Database.PostgreSQL as PG

import Monoid
import JSON
import Schema
import Global

PG.useTDBConfig "config" "postgresql"

withPG :: (PG.DBConn -> IO a) -> M a
withPG f = do
  db <- asks globalPG
  liftIO $ PG.withDB db f

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

pgIdent :: T.Text -> BSB.Builder
pgIdent = pgDQuote . TE.encodeUtf8

pgLiteral :: PGRep a => a -> BSB.Builder
pgLiteral = BSB.byteString . pgLiteralRep

pgDecodeAs :: (PGRep a, J.ToJSON a) => a -> PGValue -> J.Encoding
pgDecodeAs t v = J.toEncoding (pgDecodeRep v `asTypeOf` t)

pgDecodeType :: Type -> PGValue -> J.Encoding
pgDecodeType _ PGNullValue = JE.null_
pgDecodeType Text    v  = pgDecodeAs T.empty v
pgDecodeType Keyword v  = pgDecodeAs T.empty v
pgDecodeType Boolean v  = pgDecodeAs False v
pgDecodeType Binary  _  = error "PG Binary unsupported"
pgDecodeType Date    _  = error "PG Date unsupported"
pgDecodeType _ (PGTextValue v) = JE.unsafeToEncoding $ BSB.byteString v -- assume numeric with identical rep
pgDecodeType _ (PGBinaryValue _) = error "unexpected PG binary value"

comma :: BSB.Builder
comma = BSB.char8 ','

createTable :: Catalog -> M ()
createTable cat@Catalog{ catalogStore = CatalogPG tabn } = withPG $ \db -> do
  PG.pgExecute_ db $ BSB.toLazyByteString $ "CREATE TABLE " <> tab <> " (_id bigserial primary key " <> foldMap col fields <> ");" <> foldMap idx fields
  where
  tab = pgIdent tabn
  fields = expandFields $ catalogFields cat
  col Field{..} = comma <> pgIdent fieldName <> " " <> pgType fieldType
  idx Field{..} = "CREATE INDEX ON " <> tab <> " (" <> pgIdent fieldName <> ");"
createTable _ = return ()

checkTables :: M ()
checkTables = mapM_ check =<< asks globalCatalogs where
  check Catalog{ catalogStore = CatalogPG tabn, catalogFieldMap = maps } = do
    cols <- withPG $ \pg -> PG.pgQuery pg [pgSQL|SELECT column_name, data_type FROM information_schema.columns WHERE table_name = ${tabn}|]
    forM_ maps $ \f ->
      case lookup (Just (fieldName f)) cols of
        Just (Just t) | (t :: String) == pgType (fieldType f) -> return ()
        r -> fail $ "Missing or incorrect PG field: " ++ show tabn ++ "." ++ show (fieldName f) ++ " = " ++ show r
  check _ = return ()

queryTable :: Catalog -> Query -> M J.Encoding
queryTable Catalog{ catalogStore = CatalogPG tabn, catalogFieldMap = fieldMap } Query{..} = do
  (count : aggs, hits) <- withPG $ \pg -> do
    (_, [aggs]) <- pgSimpleQuery pg $ BSB.toLazyByteString $ "SELECT "
      <> "count(_id)" <> foldMap agg queryAggs
      <> src
    (_, hits) <- pgSimpleQuery pg $ BSB.toLazyByteString $ "SELECT "
      <> (mintersperseMap comma pgIdent fields)
      <> src
      <> (mwhen (not (null querySort)) $ " ORDER BY " <> mintersperseMap comma (\(f, a) -> pgIdent f <> if a then " ASC" else " DESC") querySort)
      <> (mwhen (queryLimit > 0) $ " LIMIT " <> BSB.wordDec queryLimit)
      <> (mwhen (queryOffset > 0) $ " OFFSET " <> BSB.wordDec queryOffset)
    return (aggs, hits)
  return $ JE.pairs $
       "aggregations" .=* aggregations queryAggs aggs
    <> "hits" .=*
      (  "total" `JE.pair` pgDecodeType Long count
      <> "hits" `JE.pair` JE.list hit hits)
  where
  fields
    | null queryFields = HM.keys fieldMap
    | otherwise = queryFields
  aggops _ = ["min", "max", "avg"]
  agg f = foldMap (\a -> comma <> a <> ("(" <> pgIdent f <> ")")) (aggops (fieldType $ fieldMap HM.! f))
  src = " FROM " <> pgIdent tabn
    <> (mwhen (not (null queryFilter)) $ " WHERE " <> mintersperseMap " AND " filt queryFilter)
  filt (f, l, Nothing) = pgIdent f <> " = " <> pgLiteral l
  filt (f, l, Just "") = pgIdent f <> " >= " <> pgLiteral l
  filt (f, "", Just u) = pgIdent f <> " <= " <> pgLiteral u
  filt (f, l,  Just u) = pgIdent f <> " BETWEEN " <> pgLiteral l <> " AND " <> pgLiteral u
  hit d = JE.pairs $ mconcat $ zipWith hitfield fields d
  hitfield f d = f `JE.pair` pgDecodeType (fieldType $ fieldMap HM.! f) d
  aggregations [] _ = mempty
  aggregations (f:fl) as = f .=* a <> aggregations fl al where
    (a, al) = aggregation (fieldType $ fieldMap HM.! f) as
  aggregation t = aggregationops t $ aggops t
  aggregationops _ [] al = (mempty, al)
  aggregationops t (o:ol) ~(a:al) = first (o `JE.pair` pgDecodeType t a <>) $ aggregationops t ol al
queryTable _ _ = return JE.null_
