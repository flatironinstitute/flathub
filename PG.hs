{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module PG
  ( createTable
  , checkTables
  , queryTable
  , queryBulk
  ) where

import           Control.Arrow (first)
import           Control.Exception (bracket)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.ByteString.Builder as BSB
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.Int (Int16, Int32, Int64)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(Proxy))
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Database.PostgreSQL.Typed (pgSQL)
import           Database.PostgreSQL.Typed.Dynamic (PGRep, pgDecodeRep, pgLiteralRep)
import           Database.PostgreSQL.Typed.Protocol (pgSimpleQuery, pgTransaction, pgPrepare, pgBind, pgFetch, pgClose)
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
-- pgType = pgTypeName $ pgTypeOf ...
pgType (Text Proxy) = "text"
pgType (Keyword Proxy) = "text"
pgType (Long Proxy) = "bigint"
pgType (Integer Proxy) = "integer"
pgType (Short Proxy) = "smallint"
pgType (Byte Proxy) = "smallint" -- XXX
pgType (Double Proxy) = "double precision"
pgType (Float Proxy) = "real"
pgType (HalfFloat Proxy) = "real" -- XXX
pgType (Boolean Proxy) = "boolean"

pgIdent :: T.Text -> BSB.Builder
pgIdent = pgDQuote . TE.encodeUtf8

pgLiteral :: PGRep a => a -> BSB.Builder
pgLiteral = BSB.byteString . pgLiteralRep

pgDecodeAs :: (PGRep a, J.ToJSON a) => a -> PGValue -> J.Value
pgDecodeAs t v = J.toJSON (pgDecodeRep v `asTypeOf` t)

pgDecodeType :: Type -> PGValue -> J.Value
pgDecodeType _ PGNullValue = J.Null
pgDecodeType (Text      Proxy) v  = pgDecodeAs T.empty v
pgDecodeType (Keyword   Proxy) v  = pgDecodeAs T.empty v
pgDecodeType (Long      Proxy) v  = pgDecodeAs (0 :: Int64) v
pgDecodeType (Integer   Proxy) v  = pgDecodeAs (0 :: Int32) v
pgDecodeType (Short     Proxy) v  = pgDecodeAs (0 :: Int16) v
pgDecodeType (Byte      Proxy) v  = pgDecodeAs (0 :: Int16) v
pgDecodeType (Double    Proxy) v  = pgDecodeAs (0 :: Double) v
pgDecodeType (Float     Proxy) v  = pgDecodeAs (0 :: Float) v
pgDecodeType (HalfFloat Proxy) v  = pgDecodeAs (0 :: Float) v
pgDecodeType (Boolean   Proxy) v  = pgDecodeAs False v
-- pgDecodeType _ (PGTextValue v) = JE.unsafeToEncoding $ BSB.byteString v -- assume numeric with identical rep
-- pgDecodeType _ (PGBinaryValue _) = error "unexpected PG binary value"

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

querySQLFrom :: T.Text -> Query -> BSB.Builder
querySQLFrom tabn Query{..} = " FROM " <> pgIdent tabn
  <> (mwhen (not (null queryFilter)) $ " WHERE " <> mintersperseMap " AND " filt queryFilter)
  where
  filt (f, l, Nothing) = pgIdent f <> " = " <> pgLiteral l
  filt (f, l, Just "") = pgIdent f <> " >= " <> pgLiteral l
  filt (f, "", Just u) = pgIdent f <> " <= " <> pgLiteral u
  filt (f, l,  Just u) = pgIdent f <> " BETWEEN " <> pgLiteral l <> " AND " <> pgLiteral u

querySQL :: BSB.Builder -> Query -> BSB.Builder
querySQL from Query{..} = "SELECT "
  <> (mintersperseMap comma pgIdent queryFields)
  <> from
  <> (mwhen (not (null querySort)) $ " ORDER BY " <> mintersperseMap comma (\(f, a) -> pgIdent f <> if a then " ASC" else " DESC") querySort)
  <> (mwhen (queryLimit > 0) $ " LIMIT " <> BSB.wordDec queryLimit)
  <> (mwhen (queryOffset > 0) $ " OFFSET " <> BSB.wordDec queryOffset)

fieldDecoder :: HM.HashMap T.Text Field -> T.Text -> PGValue -> J.Value
fieldDecoder fields f =
  pgDecodeType $ fieldType $ fields HM.! f

queryTable :: Catalog -> Query -> M J.Encoding
queryTable Catalog{ catalogStore = CatalogPG tabn, catalogFieldMap = fieldMap } query@Query{..} = do
  (count : aggs, hits) <- withPG $ \pg -> do
    (_, [aggs]) <- pgSimpleQuery pg $ BSB.toLazyByteString $ "SELECT "
      <> "count(_id)" <> foldMap agg queryAggs
      <> from
    (_, hits) <- pgSimpleQuery pg $ BSB.toLazyByteString $ querySQL from query
    return (aggs, hits)
  return $ JE.pairs $
    (mwhen (not (null aggs)) $ "aggregations" .=* aggregations queryAggs aggs)
    <> "hits" .=*
      (  "total" J..= pgDecodeType (Long Proxy) count
      <> "hits" `JE.pair` JE.list hit hits)
  where
  aggops _ = ["min", "max", "avg"]
  agg f = foldMap (\a -> comma <> a <> ("(" <> pgIdent f <> ")")) (aggops (fieldType $ fieldMap HM.! f))
  from = querySQLFrom tabn query
  hit = JE.pairs . mconcat . zipWith hitfield queryFields
  hitfield f = (f J..=) . fieldDecoder fieldMap f
  aggregations [] _ = mempty
  aggregations (f:fl) as = f .=* a <> aggregations fl al where
    (a, al) = aggregation (fieldType $ fieldMap HM.! f) as
  aggregation t = aggregationops t $ aggops t
  aggregationops _ [] al = (mempty, al)
  aggregationops t (o:ol) ~(a:al) = first (o J..= pgDecodeType t a <>) $ aggregationops t ol al
queryTable _ _ = return JE.null_

queryBulk :: Catalog -> Query -> (IO [[J.Value]] -> IO a) -> M a
queryBulk Catalog{ catalogStore = CatalogPG tabn, catalogFieldMap = fieldMap } query run =
  withPG $ \pg ->
    pgTransaction pg $ bracket
      (pgPrepare pg sql [])
      (pgClose pg)
      $ \stmt -> do
        _ <- pgBind pg stmt []
        donev <- newIORef False
        run $ do
          done <- readIORef donev
          if done
            then return []
            else do
              (r, done') <- pgFetch pg stmt 100
              writeIORef donev $ isJust done'
              return $ map decoder r
  where
  sql = BSB.toLazyByteString $ querySQL (querySQLFrom tabn query) query
  decoder = zipWith (fieldDecoder fieldMap) (queryFields query)
queryBulk _ _ run = liftIO $ run $ return []
