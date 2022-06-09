{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Api
  ( apiRoute
  , apiTop
  , apiCatalog
  , apiSchemaSQL
  , apiSchemaCSV
  , apiData
  , apiDownload
  , downloadFormats
  , apiStats
  , apiHistogram
  , apiAttachment
  , apiAttachments
  , apiAttachmentsField
  , attachmentsFormats
  , apiRoutes
  , openApi
  ) where

import           Control.Lens ((&), (&~), (.~), (?~), (%~), (.=))
import           Control.Monad (mfilter, unless, when, join, guard)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT, ask, asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import qualified Data.Attoparsec.ByteString as AP
import           Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict.InsOrd as HMI
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.List (foldl')
import           Data.Maybe (isJust, isNothing, mapMaybe, fromMaybe, catMaybes)
import qualified Data.OpenApi as OA
import           Data.Proxy (Proxy)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Vector as V
import           Data.Version (showVersion)
import           Data.Word (Word16)
import qualified Network.HTTP.Media as MT
import           Network.HTTP.Types.Header (ResponseHeaders, hOrigin, hContentType, hContentDisposition, hContentLength, hCacheControl)
import           Network.HTTP.Types.Status (ok200, noContent204, requestEntityTooLarge413, unsupportedMediaType415, unprocessableEntity422)
import qualified Network.Wai as Wai
import           Text.Read (readMaybe)
import           Waimwork.HTTP (quoteHTTP)
import           Waimwork.Response (okResponse, response)
import           Waimwork.Result (result)
import qualified Web.Route.Invertible as R
import qualified Web.Route.Invertible.Internal as RI
import qualified Web.Route.Invertible.Wai as R

import qualified Paths_flathub as Paths
import Error
import Monoid
import qualified KeyedMap as KM
import Type
import Field
import Catalog
import Global
import OpenApi
import Backend
import Output.Types
import Output.CSV
import Output.ECSV
import Output.FITS
import Output.Numpy
import Attach
import Compression

apiBase :: R.Path ()
apiBase = "api"

apiHeaders :: Wai.Request -> ResponseHeaders
apiHeaders req
  | Just origin <- lookup hOrigin (Wai.requestHeaders req) =
    [ ("access-control-allow-origin", origin) -- effectively "*" but allows us to blacklist
    , ("access-control-allow-methods", "OPTIONS, GET, POST")
    , ("access-control-allow-headers", CI.original hContentType)
    , ("access-control-max-age", "86400")
    , (hCacheControl, "public, max-age=86400")
    ]
  | otherwise = []

isDelim :: Char -> Bool
isDelim ',' = True
isDelim ' ' = True
isDelim _ = False

splitBS :: (Char -> Bool) -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitBS c s = (,) p . snd <$> BSC.uncons r
  where (p, r) = BSC.break c s

readBS :: Read a => BSC.ByteString -> Maybe a
readBS = readMaybe . BSC.unpack

decodeUtf8' :: BS.ByteString -> T.Text
decodeUtf8' = TE.decodeUtf8With TE.lenientDecode

lookupFieldQuery :: Catalog -> Bool -> BS.ByteString -> Maybe Field
lookupFieldQuery cat idx = failErr . lookupField cat idx . decodeUtf8'

fieldNameSchema :: OpenApiM (OA.Referenced OA.Schema)
fieldNameSchema = define "FieldName" $ mempty
  & OA.type_ ?~ OA.OpenApiString
  & OA.title ?~ "field name"
  & OA.description ?~ "field name in selected catalog"

fieldListSchema :: OpenApiM (OA.Referenced OA.Schema)
fieldListSchema = do
  fn <- fieldNameSchema
  define "FieldList" $ arraySchema fn
    & OA.title ?~ "field list"
    & OA.uniqueItems ?~ True

parseReadQuery :: Read a => Wai.Request -> BS.ByteString -> Maybe a
parseReadQuery req param = readMaybe . BSC.unpack =<< join (lookup param (Wai.queryString req))

parseBoolQuery :: Wai.Request -> BS.ByteString -> Maybe Bool
parseBoolQuery req param = fmap pb $ lookup param (Wai.queryString req)
  where
  pb Nothing = True
  pb (Just "") = False
  pb (Just s) = BSC.head s `elem` ("1ytYT" :: String)

parseListQuery :: Wai.Request -> BS.ByteString -> [BS.ByteString]
parseListQuery req param = foldMap sel $ Wai.queryString req where
  sel (p, v)
    | p == param = foldMap (filter (not . BSC.null) . BSC.splitWith isDelim) v
    | otherwise = []

getRequestBodyChunkLimit :: Word -> Wai.Request -> IO (IO BS.ByteString)
getRequestBodyChunkLimit n r = do
  l <- newIORef 0
  return $ do
    x <- readIORef l
    when (x > n) $ result $ response requestEntityTooLarge413 [] $ "maximum length " ++ show n
    b <- Wai.getRequestBodyChunk r
    writeIORef l (x + fromIntegral (BS.length b))
    return b

parseJSONBody :: Wai.Request -> (J.Value -> J.Parser a) -> M (Maybe a)
parseJSONBody req parse = traverse (\c -> do
  unless (c == ct) $ raise unsupportedMediaType415 $ "expecting " ++ BSC.unpack ct
  grb <- liftIO $ getRequestBodyChunkLimit 131072 req
  r <- liftIO $ AP.parseWith grb (J.json <* AP.endOfInput) BS.empty
  j <- either raise400 return $ AP.eitherResult r
  either (raise unprocessableEntity422) return $ J.parseEither parse j)
  $ lookup hContentType $ Wai.requestHeaders req
  where ct = "application/json"

data APIOp a = APIOp
  { apiName :: T.Text
  , apiSummary :: T.Text
  , apiPath :: R.Path a
  , apiExampleArg :: a
  , apiPathParams :: OpenApiM [OA.Param]
  , apiAction :: a -> Action
  , apiQueryParams :: [OpenApiM (OA.Referenced OA.Param)]
  , apiRequestSchema :: Maybe (OpenApiM OA.Schema)
  , apiResponse :: OpenApiM OA.Response
  }

-------- /api

catalogJSON :: Catalog -> J.Series
catalogJSON Catalog{..} =
     "name" J..= catalogName
  <> "order" J..= catalogOrder
  <> "title" J..= catalogTitle
  <> "synopsis" J..= catalogSynopsis
  <> "descr" J..= catalogDescr

catalogSchema :: OpenApiM (OA.Referenced OA.Schema)
catalogSchema = define "CatalogMeta" $ objectSchema
  "High-level metadata for a dataset catalog"
  [ ("name", OA.Inline $ schemaDescOf catalogName "globally unique catalog name used in urls", True)
  , ("order", OA.Inline $ schemaDescOf catalogOrder "sort key for display order", True)
  , ("title", OA.Inline $ schemaDescOf catalogTitle "display name", True)
  , ("synopsis", OA.Inline $ schemaDescOf catalogSynopsis "short description in plain text", True)
  , ("descr", OA.Inline $ schemaDescOf catalogDescr "long description in html", True)
  ]
  & OA.title ?~ "catalog metadata"

apiTop :: APIOp ()
apiTop = APIOp -- /api
  { apiName = "top"
  , apiSummary = "Get the list of available dataset catalogs"
  , apiPath = R.unit
  , apiExampleArg = ()
  , apiPathParams = return []
  , apiAction = \() req -> do
    cats <- asks globalCatalogs
    return $ okResponse (apiHeaders req)
      $ JE.list (J.pairs . catalogJSON) $ filter catalogVisible $ HM.elems $ catalogMap cats
  , apiQueryParams = []
  , apiRequestSchema = Nothing
  , apiResponse = do
    catmeta <- catalogSchema
    return $ jsonContent $ OA.Inline $ mempty
      & OA.type_ ?~ OA.OpenApiArray
      & OA.items ?~ OA.OpenApiItemsObject catmeta
      & OA.title ?~ "top result"
  }

-------- /api/{catalog}

fieldsJSON :: KM.KeyedMap (FieldSub FieldStats Proxy) -> FieldGroup -> FieldGroups -> J.Encoding
fieldsJSON stats b = JE.list fieldJSON . V.toList where
  fieldJSON f@Field{ fieldDesc = FieldDesc{..}, ..} = J.pairs
    $  "key" J..= fieldDescName
    <> "name" J..= fieldName bf
    <> "title" J..= fieldDescTitle
    <> "descr" J..= fieldDescDescr
    <> "type" J..= fieldType
    <> "base" J..= baseType ('f','i','b','s','v') fieldType
    <> mwhen (or fieldDescStore) ("store" J..= fieldDescStore)
    <> foldMap ("enum" J..=) fieldDescEnum
    <> mwhen (fieldDisp f) ("disp" J..= True)
    <> foldMap ("units" J..=) fieldDescUnits
    <> foldMap ("required" J..=) (case fieldDescFlag of
        FieldTop -> Just False
        FieldRequired -> Just True
        _ -> Nothing)
    <> mwhen fieldDescTerms ("terms" J..= fieldDescTerms)
    <> mwhen fieldDescWildcard ("wildcard" J..= fieldDescWildcard)
    <> foldMap ("dict" J..=) fieldDescDict
    <> foldMap ("scale" J..=) fieldDescScale
    <> mwhen fieldDescReversed ("reversed" J..= fieldDescReversed)
    <> mwhen (isJust fieldDescAttachment) ("attachment" J..= True)
    <> foldMap (JE.pair "stats" . fieldStatsJSON) (HM.lookup (fieldName bf) stats)
    <> foldMap (JE.pair "sub" . fieldsJSON stats bf) fieldDescSub
    where
    bf = b <> f

typeSchema :: OpenApiM (OA.Referenced OA.Schema)
typeSchema = define "Type" $ mempty
  & OA.title ?~ "field type"
  & OA.description ?~ "storage type"
  & OA.type_ ?~ OA.OpenApiString
  & OA.enum_ ?~ map J.toJSON (scalarTypes ++ map (Array . singletonArray) scalarTypes)

fieldGroupSchema :: OpenApiM (OA.Referenced OA.Schema)
fieldGroupSchema = do
  fs <- fieldStatsSchema
  ft <- typeSchema
  defineRec "FieldGroup" $ \ref -> objectSchema
    "A single field within a catalog, or a hiearchical group of fields"
    [ ("key", OA.Inline $ schemaDescOf fieldName "local name of field within this group", True)
    , ("name", OA.Inline $ schemaDescOf fieldName "global unique (\"variable\") name of field within the catalog", True)
    , ("title", OA.Inline $ schemaDescOf fieldTitle "display name of the field within the group", True)
    , ("descr", OA.Inline $ schemaDescOf fieldDescr "description of field within the group", False)
    , ("type", ft, True)
    , ("base", OA.Inline $ mempty
        & OA.description ?~ "base storage type (floating, integral, boolean, string, void)"
        & OA.type_ ?~ OA.OpenApiString
        & OA.enum_ ?~ map J.toJSON "fibsv"
        , True)
    , ("store", OA.Inline $ schemaDescOf fieldStore "true if this field is stored but not indexed, so not permitted for filtering or aggregations", False)
    , ("enum", OA.Inline $ schemaDescOf fieldEnum "if present, display values as these keywords instead (integral or boolean: enum[<int>value])", False)
    , ("disp", OA.Inline $ schemaDescOf fieldDisp "include field in data display by default", False)
    , ("units", OA.Inline $ schemaDescOf fieldUnits "display units", False)
    , ("required", OA.Inline $ schemaDescOf (True ==) "true = required filter; false = top-level (default) optional filter; missing = normal", False)
    , ("terms", OA.Inline $ schemaDescOf fieldTerms "display dynamically as a dropdown of values", False)
    , ("dict", OA.Inline $ schemaDescOf fieldDict "unique key index to global field dictionary (for compare)", False)
    , ("scale", OA.Inline $ schemaDescOf fieldScale "scale factor to dict-comparable units, display  value*scale (for compare)", False)
    , ("reversed", OA.Inline $ schemaDescOf fieldReversed "display axes and ranges in reverse (high-low)", False)
    , ("attachment", OA.Inline $ schemaDescOf isJust "this is a meta field for a downloadable attachment (type boolean, indicating presence)", False)
    , ("wildcard", OA.Inline $ schemaDescOf fieldWildcard "allow wildcard prefix searching on keyword field (\"xy*\")", False)
    , ("stats", fs, False)
    , ("sub", OA.Inline $ arraySchema ref
        & OA.title ?~ "child fields"
        & OA.description ?~ "if this is present, this is a pseudo grouping field which does not exist itself, but its properties apply to its children"
        , False)
    ]
    & OA.title ?~ "field"

catalogBase :: R.Path Simulation
catalogBase = R.parameter

catalogParam :: OA.Param
catalogParam = mempty
  & OA.name .~ "catalog"
  & OA.description ?~ "catalog name from list of catalogs"
  & OA.schema ?~ OA.Inline (schemaDescOf T.pack "catalog name")

apiCatalog :: APIOp Simulation
apiCatalog = APIOp -- /api/{cat}
  { apiName = "catalog"
  , apiSummary = "Get full metadata about a specific catalog"
  , apiPath = catalogBase
  , apiExampleArg = "catalog"
  , apiPathParams = return [catalogParam]
  , apiQueryParams = []
  , apiRequestSchema = Nothing
  , apiAction = \sim req ->
    if T.null sim then apiAction apiTop () req else do -- allow trailing slash
    cat <- askCatalog sim
    (count, stats) <- liftIO $ catalogStats cat
    return $ okResponse (apiHeaders req) $ J.pairs
      $ catalogJSON cat
      <> JE.pair "fields" (fieldsJSON stats mempty $ V.cons idField $ catalogFieldGroups cat)
      <> "count" J..= fromMaybe count (catalogCount cat)
      <> mwhen (not $ null $ catalogSort cat)
        ("sort" J..= catalogSort cat)
  , apiResponse = do
    fdef <- fieldGroupSchema
    meta <- catalogSchema
    return $ jsonContent $ OA.Inline $ mempty & OA.allOf ?~
      [ meta
      , OA.Inline $ objectSchema
        "full catalog metadata"
        [ ("fields", OA.Inline $ arraySchema fdef
          & OA.title ?~ "field groups"
          , True)
        , ("count", OA.Inline $ schemaDescOf catalogCount "total number of rows", True)
        , ("sort", OA.Inline $ schemaDescOf catalogSort "default sort fields", False)
        ]
        & OA.title ?~ "catalog info"
      ]
      & OA.title ?~ "catalog result"
  }

apiSchemaSQL :: APIOp Simulation
apiSchemaSQL = APIOp -- /api/{cat}/schema.sql
  { apiName = "schema.sql"
  , apiSummary = "Get a SQL representation of the catalog schema (no data)"
  , apiPath = catalogBase R.>* "schema.sql"
  , apiExampleArg = "catalog"
  , apiPathParams = return [catalogParam]
  , apiQueryParams = []
  , apiRequestSchema = Nothing
  , apiAction = \sim _ -> do
    cat <- askCatalog sim
    let tab = text $ catalogIndex cat
        sqls s = "$SqL$" <> s <> "$SqL$" -- hacky dangerous
    return $ Wai.responseBuilder ok200 [(hContentType, "application/sql")] $
      foldMap (\f -> foldMap (\e -> "CREATE TYPE " <> tab <> "_" <> text (fieldName f) <> " AS ENUM(" <> mintersperseMap ", " (sqls . text) (V.toList e) <> ");\n") (fieldEnum f)) (catalogFields cat)
      <> "CREATE TABLE " <> tab <> " ("
      <> mintersperseMap "," (\f -> "\n  " <> text (fieldName f) <> " " <> maybe (sqlType (fieldType f)) (\_ -> tab <> "_" <> text (fieldName f)) (fieldEnum f)) (V.toList $ catalogFields cat)
      <> foldMap (\k -> ",\n PRIMARY KEY (" <> text k <> ")") (catalogKey cat)
      <> "\n);\n"
      <> "COMMENT ON TABLE " <> tab <> " IS " <> sqls (text (catalogTitle cat) <> foldMap ((": " <>) . text) (catalogDescr cat)) <> ";\n"
      <> foldMap (\f -> "COMMENT ON COLUMN " <> tab <> "." <> text (fieldName f) <> " IS " <> sqls (text (fieldTitle f) <> foldMap ((" [" <>) . (<> "]") . text) (fieldUnits f) <> foldMap ((": " <>) . text) (fieldDescr f)) <> ";\n") (catalogFields cat)
  , apiResponse = return $ mempty
    & OA.content . at' "application/sql" . OA.schema ?~ OA.Inline (mempty
      & OA.description ?~ "CREATE TABLE SQL schema")
  }
  where
  text = TE.encodeUtf8Builder
  sqlType (Keyword _)   = "text"
  sqlType (Long _)      = "bigint"
  sqlType (ULong _)     = "bigint"
  sqlType (Integer _)   = "integer"
  sqlType (Short _)     = "smallint"
  sqlType (Byte _)      = "smallint"
  sqlType (Double _)    = "double precision"
  sqlType (Float _)     = "real"
  sqlType (HalfFloat _) = "real"
  sqlType (Boolean _)   = "boolean"
  sqlType (Void _)      = "void"
  sqlType (Array t)     = sqlType (arrayHead t) <> "[]"

apiSchemaCSV :: APIOp Simulation
apiSchemaCSV = APIOp -- /api/{cat}/schema.csv
  { apiName = "schema.csv"
  , apiSummary = "Get a CSV representation of the catalog schema (no data)"
  , apiPath = catalogBase R.>* "schema.csv"
  , apiExampleArg = "catalog"
  , apiPathParams = return [catalogParam]
  , apiQueryParams = []
  , apiRequestSchema = Nothing
  , apiAction = \sim _ -> do
    let
      populateDict cats = V.map pf $ catalogDict cats where
        pf f = f{ fieldDesc = (fieldDesc f){ fieldDescDict = Just $ T.intercalate ";" $ md (fieldName f) } }
        md d =
          [ c <> "." <> fieldName f <> foldMap (T.cons '[' . (`T.snoc` ']')) (fieldUnits f) <> foldMap (T.cons '*' . T.pack . show) (fieldScale f)
          | (c, cf) <- HM.toList (catalogMap cats)
          , f <- V.toList $ catalogFields cf
          , Just d == fieldDict f
          ]
    fields <- if sim == "dict" then asks $ populateDict . globalCatalogs else catalogFields <$> askCatalog sim
    return $ Wai.responseBuilder ok200 [(hContentType, "text/csv")]
      $ fieldsCSV fields
  , apiResponse = return $ mempty
    & OA.content . at' "text/csv" . OA.schema ?~ OA.Inline (mempty
      & OA.description ?~ "CSV representation of catalog fields")
  }
  where
  fieldsCSV l = csvTextRow ["variable", "name", "type", "units", "description", "values","dict","scale"] <> foldMap fieldCSV l
  fieldCSV f = csvTextRow
    [ fieldName f
    , fieldTitle f
    , T.pack $ show $ fieldType f
    , fold $ fieldUnits f
    , fold $ fieldDescr f
    , foldMap (T.intercalate "," . V.toList) $ fieldEnum f
    , fold $ fieldDict f
    , foldMap (T.pack . show) $ fieldScale f
    ]

-------- common query parameters

fieldValueSchema :: OpenApiM (OA.Referenced OA.Schema)
fieldValueSchema = do
  s <- define "FieldValueScalar" $ mempty
    & OA.title ?~ "scalar value"
    & OA.description ?~ "a scalar value for a field"
    & OA.anyOf ?~ map (\x -> unTypeValue (\p -> OA.Inline $ OA.toSchema p & OA.title ?~ T.pack (show x)) x) scalarTypes
  define "FieldValue" $ mempty
    & OA.title ?~ "field value"
    & OA.description ?~ "a value for a field, which must match the type of the field"
    & OA.anyOf ?~ [s, OA.Inline $ arraySchema s & OA.title ?~ "array"]

parseFilterJSON :: Typed a => Field -> J.Value -> J.Parser (FieldFilter a)
parseFilterJSON _ j@(J.Array _) = do
  l <- J.parseJSON j
  when (null l) $ fail "empty values"
  return $ FieldEQ l
parseFilterJSON f (J.Object o) | typeIsNumeric (fieldType f) = do
  g <- o J..:? "gte"
  l <- o J..:? "lte"
  unless (all (\g' -> all (g' <=) l) g) $ fail "invalid range"
  return $ FieldRange g l
parseFilterJSON f (J.Object o) | fieldWildcard f =
  FieldWildcard <$> o J..: "wildcard"
parseFilterJSON _ j = FieldEQ . return <$> J.parseJSON j

fieldFilterSchema :: OpenApiM (OA.Referenced OA.Schema)
fieldFilterSchema = do
  fv <- fieldValueSchema
  return $ OA.Inline $ mempty
    & OA.title ?~ "field filter"
    & OA.description ?~ "filter for a named field to be equal to a specific value or match other contraints"
    & OA.oneOf ?~
      [ fv
      , OA.Inline $ arraySchema fv
        & OA.title ?~ "filter equal"
        & OA.description ?~ "equal to any of these values (not supported when used as a query parameter)"
        & OA.minItems ?~ 1
        & OA.uniqueItems ?~ True
      , OA.Inline $ objectSchema "in a bounded range for numeric fields: >= gte and <= lte, either of which may be omitted (when used as a query parameter, may be a string containing two FieldValues (either of which may be blank) separated by a single comma or space); omitting both is useful for filtering out missing values"
        [ ("gte", fv, False)
        , ("lte", fv, False)
        ]
        & OA.title ?~ "filter range"
      , OA.Inline $ objectSchema "matching a pattern for wildcard fields"
        [ ("wildcard", OA.Inline $ schemaDescOf filterWildcard "a pattern containing '*' and/or '?'", True)
        ]
        & OA.title ?~ "filter wildcard"
      ]

parseFiltersJSON :: Catalog -> J.Object -> J.Parser Filters
parseFiltersJSON cat o = Filters
  <$> mfilter (\x -> x > 0 && x <= 1) (o J..:? "sample" J..!= 1)
  <*> o J..:? "seed" J..!= 0
  <*> HM.traverseWithKey parsef (HM.delete "sample" $ HM.delete "seed" o)
  where
  parsef n j = do
    f <- failErr $ lookupField cat True n
    updateFieldValueM f (parseff f j)
  parseff :: Typed a => Field -> J.Value -> Proxy a -> J.Parser (FieldFilter a)
  parseff f j _ = parseFilterJSON f j

filtersSchema :: OpenApiM (OA.Referenced OA.Schema)
filtersSchema = do
  ff <- fieldFilterSchema
  define "Filters" $ objectSchema
    "filters to apply to a query"
    [ ("sample", OA.Inline $ schemaDescOf filterSample "randomly select a fractional sample"
      & OA.default_ ?~ J.Number 1
      & OA.minimum_ ?~ 0
      & OA.exclusiveMinimum ?~ True
      & OA.maximum_ ?~ 1
      & OA.exclusiveMaximum ?~ False
      , False)
    , ("seed", OA.Inline $ schemaDescOf filterSeed "seed for random sample selection"
      & OA.default_ ?~ J.Number 0, False)
    ]
    & OA.additionalProperties ?~ OA.AdditionalPropertiesSchema ff

parseFiltersQuery :: Catalog -> Wai.Request -> Filters
parseFiltersQuery cat req = foldl' parseQueryItem mempty $ Wai.queryString req where
  parseQueryItem q ("sample", Just (readBS -> Just p)) =
    q{ filterSample = filterSample q * p }
  parseQueryItem q ("sample", Just (splitBS ('@' ==) -> Just (readBS -> Just p, readBS -> Just s))) =
    q{ filterSample = filterSample q * p, filterSeed = filterSeed q `xor` s }
  parseQueryItem q (lookupFieldQuery cat True -> Just f, Just (parseFilt f -> Just v)) =
    q{ filterFields = filterFields q <> KM.fromList [setFieldValue f $ sequenceTypeValue v] }
  parseQueryItem q _ = q -- just ignore anything we can't parse
  parseFilt f (splitBS isDelim -> Just (a, b))
    | typeIsNumeric (fieldType f) = FieldRange <$> parseVal f a <*> parseVal f b
  parseFilt f a
    | fieldWildcard f && BSC.elem '*' a = return $ FieldWildcard (decodeUtf8' a)
  parseFilt f a = FieldEQ . return <$> parseVal' f a
  parseVal _ "" = return Nothing
  parseVal f v = Just <$> parseVal' f v
  parseVal' f = fmap fieldType . parseFieldValue f . decodeUtf8'

filtersQueryParam :: OpenApiM (OA.Referenced OA.Param)
filtersQueryParam = do
  filters <- filtersSchema
  define "filters" $ mempty
    & OA.name .~ "filters"
    & OA.in_ .~ OA.ParamQuery
    & OA.style ?~ OA.StyleForm
    & OA.explode ?~ True
    & OA.schema ?~ filters
    & OA.description ?~ "filter in query string (see descriptions for non-standard formatting of range queries)"

fieldsQueryParam :: OpenApiM (OA.Referenced OA.Param)
fieldsQueryParam = do
  fl <- fieldListSchema
  define "fields" $ mempty
    & OA.name .~ "fields"
    & OA.in_ .~ OA.ParamQuery
    & OA.description ?~ "list of fields to return"
    & OA.style ?~ OA.StyleForm
    & OA.explode ?~ False
    & OA.schema ?~ fl

parseFieldsQuery :: Catalog -> Bool -> Wai.Request -> BS.ByteString -> [Field]
parseFieldsQuery cat ind req param =
  mapMaybe (lookupFieldQuery cat ind) $ parseListQuery req param

parseFieldsJSON :: Catalog -> Bool -> J.Value -> J.Parser [Field]
parseFieldsJSON cat ind = J.withArray "field list" $
  mapM (J.withText "field name" $ failErr . lookupField cat ind) . V.toList

-------- /api/{catalog}/data

parseSortQuery :: Catalog -> Wai.Request -> BS.ByteString -> [(Field, Bool)]
parseSortQuery cat req param =
  mapMaybe parseSort $ parseListQuery req param
  where
  parseSort (BSC.uncons -> Just ('+', lookf -> Just f)) = return (f, True)
  parseSort (BSC.uncons -> Just ('-', lookf -> Just f)) = return (f, False)
  parseSort (lookf -> Just f) = return (f, True)
  parseSort _ = fail "invalid sort"
  lookf = lookupFieldQuery cat True

parseSortJSON :: Catalog -> Maybe J.Value -> J.Parser [(Field, Bool)]
parseSortJSON _ Nothing = return []
parseSortJSON cat (Just sj) = J.withArray "sort" (mapM pars . V.toList) sj where
  pars :: J.Value -> J.Parser (Field, Bool)
  pars (J.String n) = (, True) <$> failErr (lookupField cat True n)
  pars (J.Object o) = (,)
    <$> (failErr . lookupField cat True =<< o J..: "field")
    <*> (mapM po =<< o J..:? "order") J..!= True
  pars j = J.typeMismatch "sort field" j
  po (J.String a)
    | T.isPrefixOf "a" a = return True
    | T.isPrefixOf "d" a = return False
  po (J.Bool b) = return b
  po j = J.typeMismatch "sort order" j

sortSchema :: OpenApiM (OA.Referenced OA.Schema)
sortSchema = do
  fn <- fieldNameSchema
  define "sort" $ arraySchema (OA.Inline $ mempty
    & OA.oneOf ?~
      [ fn
      , OA.Inline $ objectSchema "a field and an order to sort in (as a query parameter, field names may be prefixed by '+' (asc) or '-' (desc) instead)"
        [ ("field", fn, True)
        , ("order", OA.Inline $ mempty
          & OA.type_ ?~ OA.OpenApiString
          & OA.title ?~ "sort ordering"
          & OA.description ?~ "ascending smallest to largest, or descending largest to smallest"
          & OA.enum_ ?~ ["asc", "desc"]
          & OA.default_ ?~ "asc"
          & OA.pattern ?~ "^[ad]*", False)
        ]
        & OA.title ?~ "sort descriptor"
      ]
    & OA.title ?~ "sort field"
    & OA.description ?~ "fields by which to sort row data"
    & OA.uniqueItems ?~ True)
    & OA.title ?~ "sort"

sortParam :: OpenApiM (OA.Referenced OA.Param)
sortParam = do
  sort <- sortSchema
  define "sort" $ mempty
    & OA.name .~ "sort"
    & OA.in_ .~ OA.ParamQuery
    & OA.description ?~ "how to order rows (see descriptions for non-standard formatting of sort order)"
    & OA.style ?~ OA.StyleForm
    & OA.explode ?~ False
    & OA.schema ?~ sort

parseDataQuery :: Catalog -> Wai.Request -> DataArgs V.Vector
parseDataQuery cat req = DataArgs
  { dataFilters = parseFiltersQuery cat req
  , dataFields = V.fromList $ parseFieldsQuery cat False req "fields"
  , dataSort = parseSortQuery cat req "sort"
  , dataCount = fromMaybe 0 $ parseReadQuery req "count"
  , dataOffset = fromMaybe 0 $ parseReadQuery req "offset"
  }

parseDataJSON :: Catalog -> J.Value -> J.Parser (DataArgs V.Vector, Bool)
parseDataJSON cat = J.withObject "data request" $ \o -> (,) <$> (DataArgs
  <$> parseFiltersJSON cat (foldl' (flip HM.delete) o fields)
  <*> (fmap V.fromList . parseFieldsJSON cat False =<< o J..: "fields")
  <*> (parseSortJSON cat =<< o J..:? "sort")
  <*> o J..: "count"
  <*> o J..:? "offset" J..!= 0)
  <*> o J..:? "object" J..!= False
  where
  fields = ["fields", "sort", "count", "offset", "object"]

dataSchema :: OpenApiM (OA.Referenced OA.Schema)
dataSchema = do
  fv <- fieldValueSchema
  define "data" $ arraySchema (OA.Inline $ arraySchema (OA.Inline $ mempty
      & OA.oneOf ?~ [fv]
      & OA.nullable ?~ True)
    & OA.title ?~ "data row"
    & OA.description ?~ "a single data row corresponding to the requested fields in order (missing values are null)")
    & OA.title ?~ "data result"
    & OA.description ?~ "result data in the format requested, representing an array (over rows) of arrays (over values); in some formats the first row may be a list of field names"

apiData :: APIOp Simulation
apiData = APIOp -- /api/{cat}/data
  { apiName = "data"
  , apiSummary = "Get a sample of raw data rows"
  , apiPath = catalogBase R.>* "data"
  , apiExampleArg = "catalog"
  , apiPathParams = return [catalogParam]
  , apiQueryParams = [filtersQueryParam, fieldsQueryParam
    , sortParam
    , return $ OA.Inline $ mempty
      & OA.name .~ "count"
      & OA.in_ .~ OA.ParamQuery
      & OA.style ?~ OA.StyleForm
      & OA.required ?~ True
      & OA.schema ?~ countSchema
    , return $ OA.Inline $ mempty
      & OA.name .~ "offset"
      & OA.in_ .~ OA.ParamQuery
      & OA.style ?~ OA.StyleForm
      & OA.schema ?~ offsetSchema
    , return $ OA.Inline $ mempty
      & OA.name .~ "object"
      & OA.in_ .~ OA.ParamQuery
      & OA.style ?~ OA.StyleForm
      & OA.schema ?~ jobjectSchema
    ]
  , apiRequestSchema = Just $ do
    filt <- filtersSchema
    list <- fieldListSchema
    sort <- sortSchema
    return $ mempty & OA.allOf ?~
      [ filt
      , OA.Inline $ objectSchema
        "data to return"
        [ ("fields", list, True)
        , ("sort", sort, False)
        , ("count", countSchema, True)
        , ("offset", offsetSchema, False)
        , ("object", jobjectSchema, False)
        ]
      ]
  , apiAction = \sim req -> do
    cat <- askCatalog sim
    body <- parseJSONBody req (parseDataJSON cat)
    let (args, jobj) = fromMaybe (parseDataQuery cat req, fromMaybe False $ parseBoolQuery req "object") body
    okResponse (apiHeaders req) <$> if jobj
      then do
        dat <- queryData cat args{ dataFields = KM.fromList $ V.toList $ dataFields args }
        return $ J.toEncoding (dat :: V.Vector (HM.HashMap T.Text Value))
      else do
        dat <- queryData cat args
        return $ J.toEncoding (dat :: V.Vector (V.Vector (TypeValue Maybe)))
  , apiResponse = do
    ds <- dataSchema
    return $ jsonContent ds
      & OA.description .~ "selected data"
  }
  where
  countSchema = OA.Inline $ schemaDescOf dataCount "number of rows to return"
    & OA.maximum_ ?~ fromIntegral maxDataCount
  offsetSchema = OA.Inline $ schemaDescOf dataOffset "start at this row offset (0 means first)"
    & OA.maximum_ ?~ fromIntegral maxResultWindow - fromIntegral maxDataCount
    & OA.default_ ?~ J.Number 0
  jobjectSchema = OA.Inline $ schemaDescOf (True ==) "return JSON objects instead of arrays of data"
    & OA.default_ ?~ J.Bool False

-------- /api/{catalog}/data/{format}

jsonGenerator :: Wai.Request -> Catalog -> DataArgs V.Vector -> M OutputStream
jsonGenerator _ cat args = outputStreamRows Nothing
  ("[" <> J.fromEncoding (J.toEncoding (V.map fieldName (dataFields args))))
  (\j -> "," <> J.fromEncoding (J.toEncoding (j :: V.Vector (TypeValue Maybe))))
  "]"
  cat args

jsonOutput :: OutputFormat
jsonOutput = OutputFormat
  { outputMimeType = "application/json"
  , outputExtension = "json"
  , outputDescription = "JSON array data, where the first element is an array of field names and subsequent elements are arrays of values in the same order"
  , outputGenerator = jsonGenerator
  }

ndjsonGenerator :: Wai.Request -> Catalog -> DataArgs V.Vector -> M OutputStream
ndjsonGenerator _ cat args = outputStreamRows Nothing
  mempty
  (\j -> J.fromEncoding (J.toEncoding (j :: HM.HashMap T.Text Value)) <> "\n")
  mempty
  cat args{ dataFields = KM.fromList $ V.toList $ dataFields args }

ndjsonOutput :: OutputFormat
ndjsonOutput = OutputFormat
  { outputMimeType = "application/x-ndjson"
  , outputExtension = "ndjson"
  , outputDescription = "Newline-delimited JSON data, where each line is an object of field values"
  , outputGenerator = ndjsonGenerator
  }

newtype DownloadFormat = DownloadFormat OutputFormat
  deriving (KM.Keyed)

downloadFormats :: KM.KeyedMap DownloadFormat
downloadFormats = KM.fromList $ map DownloadFormat
  [ csvOutput
  , ecsvOutput
  , numpyOutput
  , fitsOutput
  , jsonOutput
  , ndjsonOutput
  ]

instance R.Parameter R.PathString DownloadFormat where
  renderParameter = KM.key
  parseParameter s = HM.lookup s downloadFormats

outputAction :: Simulation -> OutputFormat -> Maybe CompressionFormat -> (DataArgs V.Vector -> M (DataArgs V.Vector)) -> Action
outputAction sim fmt comp check req = do
  cat <- askCatalog sim
  body <- parseJSONBody req (parseDataJSON cat)
  let args = (maybe (parseDataQuery cat req) fst body){ dataCount = maxDataCount }
      enc = comp -- <|> listToMaybe (acceptCompressionEncoding req) -- transfer-encoding breaks content-length/progress
  args' <- check args
  out <- outputGenerator fmt req cat args'
  g <- ask
  return $ Wai.responseStream ok200 (apiHeaders req ++
    [ (hContentType, maybe (MT.renderHeader $ outputMimeType fmt) compressionMimeType comp)
    , (hContentDisposition, "attachment; filename=" <> quoteHTTP (TE.encodeUtf8 sim
      <> BSC.pack ('.' : outputExtension fmt ++ foldMap (('.' :) . compressionExtension) comp)))
    ] ++ catMaybes
    [ (,) hContentLength . BSC.pack . show <$> (guard (isNothing enc) >> outputSize out)
    , compressionEncodingHeader <$> (guard (enc /= comp) >> enc)
    ])
    $ compressStream enc $ \chunk _ ->
      runReaderT (C.runConduitRes $ outputStream out
        C..| C.mapM_ (liftIO . chunk)) g

apiDownload :: APIOp (Simulation, DownloadFormat, Maybe CompressionFormat)
apiDownload = APIOp
  { apiName = "download"
  , apiSummary = "Download raw data in bulk"
  , apiPath = catalogBase R.>* "data" R.>*<< R.parameter
  , apiExampleArg = ("catalog", head $ KM.toList downloadFormats, Nothing)
  , apiPathParams = return [catalogParam
    , mempty
      & OA.name .~ "format"
      & OA.schema ?~ OA.Inline (mempty
        & OA.type_ ?~ OA.OpenApiString
        & OA.enum_ ?~ [ J.toJSON (R.renderParameter (f, z) :: T.Text)
                      | f <- KM.toList downloadFormats
                      , z <- Nothing : map Just encodingCompressions
                      ])
    ]
  , apiQueryParams = [filtersQueryParam, fieldsQueryParam
    , sortParam
    ]
  , apiRequestSchema = Just $ do
    filt <- filtersSchema
    list <- fieldListSchema
    sort <- sortSchema
    return $ mempty & OA.allOf ?~
      [ filt
      , OA.Inline $ objectSchema
        "data to return"
        [ ("fields", list, True)
        , ("sort", sort, False)
        ]
      ]
  , apiAction = \(sim, DownloadFormat fmt, comp) -> outputAction sim fmt comp $ return
  , apiResponse = do
    ds <- dataSchema
    return $ mempty
      & OA.description .~ "file containing all matching content in the selected format"
      & OA.content .~ HMI.fromList
        (map (\(DownloadFormat f) -> (outputMimeType f, mempty & OA.schema ?~ OA.Inline (mempty
          & OA.title ?~ (T.pack (outputExtension f) <> " data")
          & OA.description ?~ outputDescription f
          & OA.oneOf ?~ [ds]))) (KM.toList downloadFormats)
        ++ map (\z -> (compressionMimeType z, mempty & OA.schema ?~ OA.Inline (mempty
          & OA.title ?~ (TE.decodeLatin1 (compressionEncoding z) <> " data")
          & OA.description ?~ "compressed version of the selected format"
          & OA.oneOf ?~ [ds]))) encodingCompressions)
  }

-------- /api/{catalog}/count

parseCountQuery :: Catalog -> Wai.Request -> CountArgs
parseCountQuery = parseFiltersQuery

parseCountJSON :: Catalog -> J.Value -> J.Parser CountArgs
parseCountJSON cat = J.withObject "count request" $
  parseFiltersJSON cat

apiCount :: APIOp Simulation
apiCount = APIOp -- /api/{cat}/count
  { apiName = "count"
  , apiSummary = "Get count of matching rows (given some filters)"
  , apiPath = catalogBase R.>* "count"
  , apiExampleArg = "catalog"
  , apiPathParams = return [catalogParam]
  , apiQueryParams = [filtersQueryParam]
  , apiRequestSchema = Just $ do
    filt <- filtersSchema
    return $ mempty & OA.allOf ?~
      [ filt
      ]
  , apiAction = \sim req -> do
    cat <- askCatalog sim
    body <- parseJSONBody req (parseCountJSON cat)
    count <- queryCount cat $ fromMaybe (parseCountQuery cat req) body
    return $ okResponse (apiHeaders req) $ J.toEncoding count
  , apiResponse = do
    return $ jsonContent $ OA.Inline $ schemaDescOf statsCount "number of matching rows"
      & OA.title ?~ "stats result"
  }

-------- /api/{catalog}/stats

parseStatsQuery :: Catalog -> Wai.Request -> StatsArgs
parseStatsQuery cat req = StatsArgs
  { statsFilters = parseFiltersQuery cat req
  , statsFields = KM.fromList $ parseFieldsQuery cat True req "fields"
  }

parseStatsJSON :: Catalog -> J.Value -> J.Parser StatsArgs
parseStatsJSON cat = J.withObject "stats request" $ \o -> StatsArgs
  <$> parseFiltersJSON cat (HM.delete "fields" o)
  <*> (maybe (return KM.empty) (fmap KM.fromList . parseFieldsJSON cat True) =<< o J..:? "fields")

fieldStatsJSON :: FieldSub FieldStats m -> J.Encoding
fieldStatsJSON = unTypeValue fsj . fieldType
  where
  fsj :: J.ToJSON a => FieldStats a -> J.Encoding
  fsj FieldStats{..} = J.pairs
    $  "count" J..= statsCount
    <> "min" J..= statsMin
    <> "max" J..= statsMax
    <> "avg" J..= statsAvg
  fsj FieldTerms{..} = J.pairs
    $  "terms" `JE.pair` JE.list (\(v,c) -> J.pairs $ "value" J..= v <> "count" J..= c) termsBuckets
    <> "others" J..= termsCount

fieldStatsSchema :: OpenApiM (OA.Referenced OA.Schema)
fieldStatsSchema = do
  fv <- fieldValueSchema
  define "FieldStats" $ mempty
    & OA.title ?~ "field stats"
    & OA.description ?~ "stats for the field named by the property, depending on its type"
    & OA.oneOf ?~
      [ OA.Inline $ objectSchema "for numeric fields"
        [ ("count", OA.Inline $ schemaDescOf statsCount "number of rows with values for this field", True)
        , ("min", OA.Inline $ schemaDescOf statsMin "minimum value" & OA.nullable ?~ True, True)
        , ("max", OA.Inline $ schemaDescOf statsMax "maximum value" & OA.nullable ?~ True, True)
        , ("avg", OA.Inline $ schemaDescOf statsAvg "mean value" & OA.nullable ?~ True, True)
        ]
        & OA.title ?~ "numeric stats"
      , OA.Inline $ objectSchema "for non-numeric fields or those with terms=true"
        [ ("terms", OA.Inline $ arraySchema (OA.Inline $ objectSchema "unique field value"
          [ ("value", fv, True)
          , ("count", OA.Inline $ schemaDescOf termsCount "number of rows with this value", True)
          ] & OA.title ?~ "top term")
          & OA.title ?~ "top terms"
          & OA.description ?~ "top terms in descending order of count"
          & OA.uniqueItems ?~ True, True)
        , ("others", OA.Inline $ schemaDescOf termsCount "number of rows with values not included in the top terms", True)
        ]
      ]

apiStats :: APIOp Simulation
apiStats = APIOp -- /api/{cat}/stats
  { apiName = "stats"
  , apiSummary = "Get statistics about fields (given some filters)"
  , apiPath = catalogBase R.>* "stats"
  , apiExampleArg = "catalog"
  , apiPathParams = return [catalogParam]
  , apiQueryParams = [filtersQueryParam, fieldsQueryParam]
  , apiRequestSchema = Just $ do
    filt <- filtersSchema
    list <- fieldListSchema
    return $ mempty & OA.allOf ?~
      [ filt
      , OA.Inline $ objectSchema
        "stats to request"
        [ ("fields", list, False)
        ]
        & OA.title ?~ "stats fields"
      ]
  , apiAction = \sim req -> do
    cat <- askCatalog sim
    body <- parseJSONBody req (parseStatsJSON cat)
    (count, stats) <- queryStats cat $ fromMaybe (parseStatsQuery cat req) body
    return $ okResponse (apiHeaders req) $ J.pairs
      $  "count" J..= count
      <> foldMap (\f -> fieldName f `JE.pair` fieldStatsJSON f) stats
  , apiResponse = do
    fs <- fieldStatsSchema
    return $ jsonContent $ OA.Inline $ objectSchema "stats"
      [ ("count", OA.Inline $ schemaDescOf statsCount "number of matching rows", True) ]
      & OA.additionalProperties ?~ OA.AdditionalPropertiesSchema fs
      & OA.title ?~ "stats result"
  }

-------- /api/{catalog}/histogram

defaultHistogramSize :: Word16
defaultHistogramSize = 16

histogramSchema :: OpenApiM (OA.Referenced OA.Schema)
histogramSchema = do
  fn <- fieldNameSchema
  define "Histogram" $ mempty & OA.oneOf ?~
    [ fn
    , OA.Inline $ objectSchema "parameters for a single-field histogram (when used as a query parameter, may be specified as \"FIELD:[log]SIZE\"); it's recommended to include fully-bounded range filters for any histogram fields"
      [ ("field", fn, True)
      , ("size", OA.Inline $ schemaDescOf histogramSize "number of buckets to include in the histogram"
        & OA.title ?~ "histogram size"
        & OA.maximum_ ?~ fromIntegral maxHistogramSize
        & OA.default_ ?~ J.Number (fromIntegral defaultHistogramSize), False)
      , ("log", OA.Inline $ schemaDescOf histogramLog "whether to calculate the histogram using log-spaced buckets (rather than linear spacing)"
        & OA.title ?~ "histogram scale"
        & OA.default_ ?~ J.Bool False, False)
      ]
      & OA.title ?~ "histogram desc"
    ]
    & OA.title ?~ "histogram field"

histogramListSchema :: OpenApiM (OA.Referenced OA.Schema)
histogramListSchema = do
  hist <- histogramSchema
  define "HistogramList" $ mempty & OA.oneOf ?~
    [ hist
    , OA.Inline $ arraySchema hist
      & OA.uniqueItems ?~ True
      & OA.minItems ?~ 1
      & OA.maxItems ?~ fromIntegral maxHistogramDepth
      & OA.description ?~ "fields (dimensions or axes) along which to calculate a histogram, where each bucket will represent an intersection of all the fields and count of rows in that box, as calculated by the nested conditional histograms of inner (later) fields within outer (earlier) histograms"
    ]
    & OA.title ?~ "histogram fields"

parseHistogramsQuery :: Catalog -> Wai.Request -> BS.ByteString -> [Histogram]
parseHistogramsQuery cat req param =
  mapMaybe parseHist $ parseListQuery req param
  where
  parseHist (splitBS (':' ==) -> Just (lookf -> Just f, histn -> Just n)) = mkHist f n
  parseHist (                          lookf -> Just f                  ) = mkHist f (False, defaultHistogramSize)
  parseHist _ = fail "invalid hist"
  histn s = (t, ) <$> readBS r where
    (t, r) = maybe (False, s) (True ,) $ BS.stripPrefix "log" s
  mkHist f (t, n)
    | typeIsNumeric (fieldType f) = return $ Histogram f n t
    | otherwise = fail "non-numeric hist"
  lookf = lookupFieldQuery cat True

parseHistogramsJSON :: Catalog -> J.Value -> J.Parser Histogram
parseHistogramsJSON cat (J.String s) = do
  f <- failErr $ lookupField cat True s
  return $ Histogram f defaultHistogramSize False
parseHistogramsJSON cat (J.Object o) = Histogram
  <$> (failErr . lookupField cat True =<< o J..: "field")
  <*> o J..:? "size" J..!= defaultHistogramSize
  <*> o J..:? "log" J..!= False
parseHistogramsJSON _ j = J.typeMismatch "histogram field" j

parseHistogramQuery :: Catalog -> Wai.Request -> HistogramArgs
parseHistogramQuery cat req = HistogramArgs
  { histogramFilters = parseFiltersQuery cat req
  , histogramFields = parseHistogramsQuery cat req "fields"
  , histogramQuartiles = lookupFieldQuery cat True =<< join (lookup "quartiles" $ Wai.queryString req)
  }

parseHistogramJSON :: Catalog -> J.Value -> J.Parser HistogramArgs
parseHistogramJSON cat = J.withObject "histogram request" $ \o -> HistogramArgs
  <$> parseFiltersJSON cat (HM.delete "fields" $ HM.delete "quartiles" o)
  <*> (mapM (parseHistogramsJSON cat) =<< o J..: "fields")
  <*> (mapM (failErr . lookupField cat True) =<< o J..:? "quartiles")

apiHistogram :: APIOp Simulation
apiHistogram = APIOp -- /api/{cat}/histogram
  { apiName = "histogram"
  , apiSummary = "Get a histogram of data across one or more fields"
  , apiPath = catalogBase R.>* "histogram"
  , apiExampleArg = "catalog"
  , apiPathParams = return [catalogParam]
  , apiQueryParams = [filtersQueryParam
    , histogramListSchema >>= \hist -> return $ OA.Inline $ mempty
      & OA.name .~ "fields"
      & OA.in_ .~ OA.ParamQuery
      & OA.description ?~ "field(s) along which to calculate histograms (see descriptions for non-standard formatting of size/log)"
      & OA.style ?~ OA.StyleForm
      & OA.explode ?~ False
      & OA.required ?~ True
      & OA.schema ?~ hist
    , fieldNameSchema >>= \fn -> return $ OA.Inline $ mempty
      & OA.name .~ "quartiles"
      & OA.in_ .~ OA.ParamQuery
      & OA.description ?~ "optional field within which to calculate quartiles"
      & OA.schema ?~ fn
    ]
  , apiRequestSchema = Just $ do
    filt <- filtersSchema
    fn <- fieldNameSchema
    hist <- histogramListSchema
    return $ mempty & OA.allOf ?~
      [ filt
      , OA.Inline $ objectSchema
        "histogram axes and an optional quartiles axis for which quartiles are calculated in each inner-most histogram bucket"
        [ ("fields", hist, True)
        , ("quartiles", fn, False)
        ]
        & OA.title ?~ "histogram parameters"
      ]
  , apiAction = \sim req -> do
    cat <- askCatalog sim
    body <- parseJSONBody req (parseHistogramJSON cat)
    let args = fromMaybe (parseHistogramQuery cat req) body
    HistogramResult{..} <- queryHistogram cat args
    return $ okResponse (apiHeaders req) $ J.pairs
      $  "sizes" J..= histogramSizes
      <> "buckets" `JE.pair` JE.list (\HistogramBucket{..} -> J.pairs
        $  "key" `JE.pair` J.foldable bucketKey
        <> "count" J..= bucketCount
        <> foldMap ("quartiles" J..=) bucketQuartiles)
        histogramBuckets
  , apiResponse = do
    fv <- fieldValueSchema
    return $ jsonContent $ OA.Inline $ objectSchema "histogram result"
      [ ("sizes", OA.Inline $ arraySchema (OA.Inline $ schemaDescOf (head . histogramSizes) "size of each histogram buckets in this dimension, either as an absolute width in linear space, or as a ratio in log space"
          & OA.title ?~ "bucket size"
          & OA.minimum_ ?~ 0
          & OA.exclusiveMinimum ?~ True)
        & OA.title ?~ "bucket dimensions"
        & OA.description ?~ "field order corresponds to the requested histogram fields and bucket keys", True)
      , ("buckets", OA.Inline $ arraySchema $ OA.Inline $ objectSchema "histogram bucket, representing a box in the dimension space specified by the requested fields"
        [ ("key", OA.Inline $ arraySchema fv
          & OA.description ?~ "the minimum (left) point of this bucket, such than the bucket includes the range [key,key+size) (or [key,key*size) for log scale)", True)
        , ("count", OA.Inline $ schemaDescOf bucketCount "the number of rows with values that fall within this bucket", True)
        , ("quartiles", OA.Inline $ arraySchema fv
          & OA.description ?~ "if quartiles of a field were requested, includes the values of that field corresponding to the [0,25,50,75,100] percentiles ([min, first quartile, median, third quartile, max]) for rows within this bucket", False)
        ] & OA.title ?~ "bucket", True)
      ]
      & OA.title ?~ "histogram result"
  }

-------- /api/{cat}/attachment/{field}/{id}

apiAttachment :: APIOp (Simulation, T.Text, T.Text)
apiAttachment = APIOp
  { apiName = "attachment"
  , apiSummary = "Download a row attachment"
  , apiPath = catalogBase R.>* "attachment" R.>*<< R.parameter R.>*< R.parameter
  , apiExampleArg = ("catalog", "field", "rid")
  , apiPathParams = do
    fn <- fieldNameSchema
    return [catalogParam
      , mempty
        & OA.name .~ "field"
        & OA.schema ?~ fn
        & OA.description ?~ "field name of attachment"
      , mempty
        & OA.name .~ "id"
        & OA.schema ?~ OA.Inline (schemaDescOf T.pack "field value")
        & OA.description ?~ "_id for row of interest"
      ]
  , apiQueryParams = []
  , apiRequestSchema = Nothing
  , apiAction = \(sim, atn, rid) _ -> do
    cat <- askCatalog sim
    atf <- lookupField cat False atn
    att <- maybe (raise404 "Not attachment field") return $ fieldAttachment atf
    dat <- queryData cat DataArgs
      { dataFilters = mempty{ filterFields = KM.singleton $
        idField{ fieldType = Keyword (FieldEQ [rid]) } }
      , dataCount = 1
      , dataFields = KM.fromList $ attachmentFields cat atf
      , dataSort = []
      , dataOffset = 0
      }
    doc <- maybe (raise404 "Attachment row not found") return $
      mfilter (any attachmentPresent . HM.lookup atn) (dat V.!? 0)
    uncurry attachmentResponse $ resolveAttachment doc att
  , apiResponse = return $ mempty
    & OA.content . at' "application/octet-stream" . OA.schema ?~ OA.Inline (mempty
      & OA.description ?~ "attachment file (type depends on field)")
  }

-------- /api/{cat}/attachments/{format}

newtype AttachmentsFormat = AttachmentsFormat OutputFormat
  deriving (KM.Keyed)

attachmentsFormats :: KM.KeyedMap AttachmentsFormat
attachmentsFormats = KM.fromList $ map AttachmentsFormat
  [ zipAttachments
  , listAttachments $ apiRoute apiAttachment
  , curlAttachments $ apiRoute apiAttachment
  ]

instance R.Parameter R.PathString AttachmentsFormat where
  renderParameter = KM.key
  parseParameter s = HM.lookup s attachmentsFormats

attachmentsResponse :: OA.Response
attachmentsResponse = mempty
  & OA.description .~ "file containing all matching attachments in the selected format"
  & OA.content .~ HMI.fromList
    (map (\(AttachmentsFormat f) -> (outputMimeType f, mempty & OA.schema ?~ OA.Inline (mempty
      & OA.title ?~ (T.pack (outputExtension f) <> " attachments")
      & OA.description ?~ outputDescription f)))
      (KM.toList attachmentsFormats))

apiAttachments :: APIOp (Simulation, AttachmentsFormat)
apiAttachments = APIOp
  { apiName = "attachments"
  , apiSummary = "Download attachments in bulk from matching rows for multiple fields"
  , apiPath = catalogBase R.>* "attachments" R.>*< R.parameter
  , apiExampleArg = ("catalog", head $ KM.toList attachmentsFormats)
  , apiPathParams = return [catalogParam
    , mempty
      & OA.name .~ "format"
      & OA.schema ?~ OA.Inline (mempty
        & OA.type_ ?~ OA.OpenApiString
        & OA.enum_ ?~ map (J.toJSON . KM.key) (KM.toList attachmentsFormats))
    ]
  , apiQueryParams = [filtersQueryParam
    , fieldsQueryParam]
  , apiRequestSchema = Just $ do
    filt <- filtersSchema
    list <- fieldListSchema
    return $ mempty & OA.allOf ?~
      [ filt
      , OA.Inline $ objectSchema
        "attachments to return; use fields parameter to specify which attachments to include, and filter to select which rows"
        [ ("fields", list, True)
        ]
      ]
  , apiAction = \(sim, AttachmentsFormat fmt) -> outputAction sim fmt Nothing $ \args -> do
    return args
      { dataFields = V.filter (isJust . fieldAttachment) (dataFields args)
      }
  , apiResponse = return attachmentsResponse
  }

apiAttachmentsField :: APIOp (Simulation, AttachmentsFormat, T.Text)
apiAttachmentsField = APIOp
  { apiName = "attachments1"
  , apiSummary = "Download attachments in bulk from matching rows for single field"
  , apiPath = catalogBase R.>* "attachments" R.>*< R.parameter R.>>*< R.parameter
  , apiExampleArg = ("catalog", head $ KM.toList attachmentsFormats, "field")
  , apiPathParams = do
    fn <- fieldNameSchema
    return
      [ catalogParam
      , mempty
        & OA.name .~ "format"
        & OA.schema ?~ OA.Inline (mempty
          & OA.type_ ?~ OA.OpenApiString
          & OA.enum_ ?~ map (J.toJSON . KM.key) (KM.toList attachmentsFormats))
      , mempty
        & OA.name .~ "field"
        & OA.schema ?~ fn
      ]
  , apiQueryParams = [filtersQueryParam]
  , apiRequestSchema = Just $ do
    filt <- filtersSchema
    return $ mempty & OA.allOf ?~
      [ filt
      ]
  , apiAction = \(sim, AttachmentsFormat fmt, atn) -> outputAction sim fmt Nothing $ \args -> do
    cat <- askCatalog sim
    atf <- lookupField cat False atn
    unless (isJust (fieldAttachment atf)) $
      raise404 "Not attachment field"
    return args{ dataFields = V.singleton atf }
  , apiResponse = return attachmentsResponse
  }

-------- global

apiRoute :: APIOp a -> R.Route a
apiRoute op = R.routePath (apiBase R.*< apiPath op)

apiRouteAction :: APIOp a -> R.RouteAction (R.Method, a) Action
apiRouteAction op =
  R.RouteAction (R.routeMethods [R.GET, R.POST, R.OPTIONS] R.>*< apiRoute op) act where
  act (R.OPTIONS, _) req = return $ response noContent204 (apiHeaders req) ()
  act (_, x) req = apiAction op x req

data AnyAPIOp = forall a . AnyAPIOp (APIOp a)

apiOps :: [AnyAPIOp]
apiOps =
  [ AnyAPIOp apiTop
  , AnyAPIOp apiCatalog
  , AnyAPIOp apiSchemaSQL
  , AnyAPIOp apiSchemaCSV
  , AnyAPIOp apiData
  , AnyAPIOp apiDownload
  , AnyAPIOp apiCount
  , AnyAPIOp apiStats
  , AnyAPIOp apiHistogram
  , AnyAPIOp apiAttachment
  , AnyAPIOp apiAttachments
  , AnyAPIOp apiAttachmentsField
  ]

apiRoutes :: [R.RouteCase Action]
apiRoutes = map (\(AnyAPIOp op) -> R.routeNormCase $ apiRouteAction op) apiOps

openApiBase :: OA.OpenApi
openApiBase = mempty &~ do
  OA.info .= (mempty
    & OA.title .~ "FlatHUB API"
    & OA.version .~ T.pack (showVersion Paths.version)
    & OA.description ?~ "Most operations support GET and POST, either of which accepts JSON request bodies or query parameters.  In most cases, query parameters are ignored when there is a request body.")
  OA.servers .=
    [ OA.Server (routeRequestUrl $ baseApiRequest mempty{ R.requestSecure = True, R.requestHost = RI.splitHost "flathub.flatironinstitute.org" }) (Just "production") mempty
    ]

  mapM_ (\(AnyAPIOp APIOp{..}) -> do
    pparam <- apiPathParams
    qparam <- sequence apiQueryParams
    reqs <- sequence apiRequestSchema
    res <- apiResponse
    let path = '/' : mintersperseMap "/" T.unpack (pathPlaceholders apiPath apiExampleArg pparam)
        pparam' = map (\p -> OA.Inline $ p
          & OA.in_ .~ OA.ParamPath
          & OA.required ?~ True) pparam
    op <- (OA.parameters .~ pparam') <$>
      makeOp apiName apiSummary (res & OA.description %~ (\d -> if T.null d then apiName <> " result" else d))
    OA.paths . at' path .= (mempty
      & OA.get ?~ (op
        & OA.parameters %~ (++ qparam))
      & OA.post .~ fmap (\r -> op
        & OA.operationId %~ (fmap (<>"POST"))
        & OA.requestBody ?~ OA.Inline (jsonContent $ OA.Inline (r & OA.title ?~ (apiName <> " request")))) reqs))
    apiOps

openApi :: Route ()
openApi = getPath "openapi.json" $ \() req ->
  return $ okResponse (apiHeaders req) $ J.toEncoding $ openApiBase
    & OA.servers %~ (OA.Server (routeRequestUrl $ baseApiRequest $ R.waiRequest req) Nothing mempty :)

baseApiRequest :: R.Request -> R.Request
baseApiRequest = RI.requestRoute' (R.routePath apiBase) ()
