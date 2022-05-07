{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api
  ( apiRoutes
  , openApi
  ) where

import           Control.Lens ((&), (&~), (.~), (?~), (%~), (.=))
import           Control.Monad (mfilter, unless, when, join)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import qualified Data.Attoparsec.ByteString as AP
import           Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.List (foldl')
import           Data.Maybe (isJust, mapMaybe, fromMaybe)
import qualified Data.OpenApi as OA
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Vector as V
import           Data.Version (showVersion)
import           Data.Word (Word16)
import           Network.HTTP.Types.Header (ResponseHeaders, hOrigin, hContentType)
import           Network.HTTP.Types.Status (noContent204, requestEntityTooLarge413, unsupportedMediaType415, unprocessableEntity422)
import qualified Network.Wai as Wai
import           Text.Read (readMaybe)
import           Waimwork.Response (okResponse, response)
import           Waimwork.Result (result)
import qualified Web.Route.Invertible as R
import qualified Web.Route.Invertible.Internal as RI
import qualified Web.Route.Invertible.Wai as R

import qualified Paths_flathub as Paths
import Monoid
import qualified KeyedMap as KM
import Type
import Field
import Catalog
import Global
import OpenApi
import Backend

apiBase :: R.Path ()
apiBase = "api"

apiHeaders :: Wai.Request -> ResponseHeaders
apiHeaders req
  | Just origin <- lookup hOrigin (Wai.requestHeaders req) =
    [ ("access-control-allow-origin", origin) -- effectively "*" but allows us to blacklist
    , ("access-control-allow-methods", "OPTIONS, GET, POST")
    , ("access-control-allow-headers", CI.original hContentType)
    , ("access-control-max-age", "86400")
    ]
  | otherwise = []

apiRouteAction :: R.Path a -> (a -> Action) -> R.RouteAction (R.Method, a) Action
apiRouteAction p f =
  R.RouteAction (R.routeMethods [R.GET, R.POST, R.OPTIONS] R.>*< R.routePath (apiBase R.*< p)) act where
  act (R.OPTIONS, _) req = return $ response noContent204 (apiHeaders req) ()
  act (_, x) req = f x req

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

lookupFieldQuery :: MonadFail m => Catalog -> Bool -> BS.ByteString -> m Field
lookupFieldQuery cat idx = lookupField cat idx . decodeUtf8'

fieldNameSchema :: OpenApiM (OA.Referenced OA.Schema)
fieldNameSchema = define "FieldName" $ mempty
  & OA.type_ ?~ OA.OpenApiString
  & OA.description ?~ "field name in selected catalog"

fieldListSchema :: OpenApiM (OA.Referenced OA.Schema)
fieldListSchema = do
  fn <- fieldNameSchema
  define "FieldList" $ arraySchema fn
    & OA.uniqueItems ?~ True

parseReadQuery :: Read a => Wai.Request -> BS.ByteString -> Maybe a
parseReadQuery req param = readMaybe . BSC.unpack =<< join (lookup param (Wai.queryString req))

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

data APIOperation = forall a . APIOperation
  { apiName :: T.Text
  , apiSummary :: T.Text
  , apiPath :: R.Path a
  , apiExampleArg :: a
  , apiPathParams :: [OA.Param]
  , apiAction :: a -> Action
  , apiQueryParams :: [OpenApiM (OA.Referenced OA.Param)]
  , apiRequestSchema :: Maybe (OpenApiM OA.Schema)
  , apiResponseSchema :: OpenApiM OA.Schema
  }

-------- /api

catalogJSON :: Catalog -> J.Series
catalogJSON Catalog{..} =
     "name" J..= catalogName
  <> "order" J..= catalogOrder
  <> "title" J..= catalogTitle
  <> "synopsis" J..= catalogSynopsis

instance OA.ToSchema Catalog where
  declareNamedSchema _ = do
    return $ OA.NamedSchema (Just "CatalogMeta") $ objectSchema
      "High-level metadata for a dataset catalog"
      [ ("name", OA.Inline $ schemaDescOf catalogName "globally unique catalog id used in urls", True)
      , ("order", OA.Inline $ schemaDescOf catalogOrder "sort key for display order", True)
      , ("title", OA.Inline $ schemaDescOf catalogTitle "display name", True)
      , ("synopsis", OA.Inline $ schemaDescOf catalogSynopsis "short description", True)
      ]

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

instance OA.ToSchema Type where
  declareNamedSchema _ = do
    return $ OA.NamedSchema (Just "Type") $ mempty
      & OA.description ?~ "storage type"
      & OA.type_ ?~ OA.OpenApiString
      & OA.enum_ ?~ map J.toJSON allTypes

instance OA.ToSchema FieldGroup where
  declareNamedSchema t = do
    ref <- OA.declareSchemaRef t
    fs <- OA.declareSchemaRef (Proxy :: Proxy (FieldStats Void))
    return $ OA.NamedSchema (Just "FieldGroup") $ objectSchema
      "A single field within a catalog, or a hiearchical group of fields"
      [ ("key", OA.Inline $ schemaDescOf fieldName "local name of field within this group", True)
      , ("name", OA.Inline $ schemaDescOf fieldName "global unique (\"variable\") name of field within the catalog", True)
      , ("title", OA.Inline $ schemaDescOf fieldTitle "display name of the field within the group", True)
      , ("descr", OA.Inline $ schemaDescOf fieldDescr "description of field within the group", False)
      , ("type", OA.Inline $ schemaDescOf typeOfValue "raw storage type", True)
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
          & OA.description ?~ "child fields: if this is present, this is a pseudo grouping field which does not exist itself, but its properties apply to its children"
          , False)
      ]

catalogBase :: R.Path Simulation
catalogBase = R.parameter

catalogParam :: OA.Param
catalogParam = mempty
  & OA.name .~ "sim"
  & OA.schema ?~ OA.Inline (schemaDescOf T.pack "catalog id")

-------- common query parameters

instance OA.ToSchema FieldValue where
  declareNamedSchema _ = do
    return $ OA.NamedSchema (Just "FieldValue") $ mempty
      & OA.description ?~ "a value for a field, which must match the type of the field"
      & OA.anyOf ?~ map (\t -> unTypeValue (\p -> OA.Inline $ OA.toSchema p & OA.title ?~ T.pack (show t)) t) allTypes

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

instance Typed a => OA.ToSchema (FieldFilter a) where
  declareNamedSchema _ = do
    fv <- OA.declareSchemaRef (Proxy :: Proxy FieldValue)
    return $ OA.NamedSchema Nothing $ mempty
      & OA.description ?~ "filter for a named field to be equal to a specific value or match other contraints"
      & OA.oneOf ?~
        [ fv
        , OA.Inline $ arraySchema fv & OA.description ?~ "equal to any of these values (not supported when used as a query parameter)"
          & OA.minItems ?~ 1
          & OA.uniqueItems ?~ True
        , OA.Inline $ objectSchema "in a bounded range for numeric fields: >= gte and <= lte, either of which may be omitted (when used as a query parameter, may be a string containing two FieldValues (either of which may be blank) separated by a single comma or space); omitting both is useful for filtering out missing values"
          [ ("gte", fv, False)
          , ("lte", fv, False)
          ]
        , OA.Inline $ objectSchema "matching a pattern for wildcard fields"
          [ ("wildcard", OA.Inline $ schemaDescOf filterWildcard "a pattern containing '*' and/or '?'", True)
          ]
        ]

parseFiltersJSON :: Catalog -> J.Object -> J.Parser Filters
parseFiltersJSON cat o = Filters
  <$> mfilter (\x -> x > 0 && x <= 1) (o J..:? "sample" J..!= 1)
  <*> o J..:? "seed"
  <*> HM.traverseWithKey parsef (HM.delete "sample" $ HM.delete "seed" o)
  where
  parsef n j = do
    f <- lookupField cat True n
    updateFieldValueM f (parseff f j)
  parseff :: Typed a => Field -> J.Value -> Proxy a -> J.Parser (FieldFilter a)
  parseff f j _ = parseFilterJSON f j

instance OA.ToSchema Filters where
  declareNamedSchema _ = do
    ff <- OA.declareSchemaRef (Proxy :: Proxy (FieldFilter Void))
    return $ OA.NamedSchema (Just "Filters") $ objectSchema
      "filters to apply to a query"
      [ ("sample", OA.Inline $ schemaDescOf filterSample "randomly select a fractional sample"
        & OA.default_ ?~ J.Number 1
        & OA.minimum_ ?~ 0
        & OA.exclusiveMinimum ?~ True
        & OA.maximum_ ?~ 1
        & OA.exclusiveMaximum ?~ False
        , False)
      , ("seed", OA.Inline $ schemaDescOf filterSeed "seed for random sample selection (defaults to new random seed)", False)
      ]
      & OA.additionalProperties ?~ OA.AdditionalPropertiesSchema ff

parseFiltersQuery :: Catalog -> Wai.Request -> Filters
parseFiltersQuery cat req = foldl' parseQueryItem mempty $ Wai.queryString req where
  parseQueryItem q ("sample", Just (readBS -> Just p)) =
    q{ filterSample = filterSample q * p }
  parseQueryItem q ("sample", Just (splitBS ('@' ==) -> Just (readBS -> Just p, readBS -> Just s))) =
    q{ filterSample = filterSample q * p, filterSeed = Just $ maybe id xor (filterSeed q) s }
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
  filters <- declareSchemaRef (Proxy :: Proxy Filters)
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

parseFieldsQuery :: Catalog -> Wai.Request -> BS.ByteString -> [Field]
parseFieldsQuery cat req param =
  mapMaybe (lookupFieldQuery cat False) $ parseListQuery req param

parseFieldsJSON :: Catalog -> J.Value -> J.Parser [Field]
parseFieldsJSON cat = J.withArray "field list" $
  mapM (J.withText "field name" $ lookupField cat False) . V.toList

-------- /api/{catalog}/stats

parseStatsQuery :: Catalog -> Wai.Request -> StatsArgs
parseStatsQuery cat req = StatsArgs
  { statsFilters = parseFiltersQuery cat req
  , statsFields = parseFieldsQuery cat req "fields"
  }

parseStatsJSON :: Catalog -> J.Value -> J.Parser StatsArgs
parseStatsJSON cat = J.withObject "stats request" $ \o -> StatsArgs
  <$> parseFiltersJSON cat (HM.delete "fields" o)
  <*> (maybe (return []) (parseFieldsJSON cat) =<< o J..:? "fields")

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

instance Typed a => OA.ToSchema (FieldStats a) where
  declareNamedSchema _ = do
    fv <- OA.declareSchemaRef (Proxy :: Proxy FieldValue)
    return $ OA.NamedSchema (Just "FieldStats") $ mempty
      & OA.oneOf ?~
        [ OA.Inline $ objectSchema "for numeric fields"
          [ ("count", OA.Inline $ schemaDescOf statsCount "number of rows with values for this field", True)
          , ("min", OA.Inline $ schemaDescOf statsMin "minimum value" & OA.nullable ?~ True, True)
          , ("max", OA.Inline $ schemaDescOf statsMax "maximum value" & OA.nullable ?~ True, True)
          , ("avg", OA.Inline $ schemaDescOf statsAvg "mean value" & OA.nullable ?~ True, True)
          ]
        , OA.Inline $ objectSchema "for non-numeric fields or those with terms=true"
          [ ("terms", OA.Inline $ arraySchema (OA.Inline $ objectSchema "unique field value"
            [ ("value", fv, True)
            , ("count", OA.Inline $ schemaDescOf termsCount "number of rows with this value", True)
            ])
            & OA.description ?~ "top terms in descending order of count"
            & OA.uniqueItems ?~ True, True)
          , ("others", OA.Inline $ schemaDescOf termsCount "number of rows with values not included in the top terms", True)
          ]
        ]
      & OA.description ?~ "stats for the field named by the property, depending on its type"

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
  pars (J.String n) = (, True) <$> lookupField cat True n
  pars (J.Object o) = (,)
    <$> (lookupField cat True =<< o J..: "field")
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
          & OA.description ?~ "sort order: ascending smallest to largest, or descending largest to smallest"
          & OA.enum_ ?~ ["asc", "desc"]
          & OA.default_ ?~ "asc"
          & OA.pattern ?~ "^[ad]*", False)
        ]
      ]
    & OA.description ?~ "fields by which to sort row data"
    & OA.uniqueItems ?~ True)

parseDataQuery :: Catalog -> Wai.Request -> DataArgs
parseDataQuery cat req = DataArgs
  { dataFilters = parseFiltersQuery cat req
  , dataFields = parseFieldsQuery cat req "fields"
  , dataSort = parseSortQuery cat req "sort"
  , dataCount = fromMaybe 0 $ parseReadQuery req "count"
  , dataOffset = fromMaybe 0 $ parseReadQuery req "offset"
  }

parseDataJSON :: Catalog -> J.Value -> J.Parser DataArgs
parseDataJSON cat = J.withObject "data request" $ \o -> DataArgs
  <$> parseFiltersJSON cat (HM.delete "fields" $ HM.delete "sort" $ HM.delete "count" $ HM.delete "offset" o)
  <*> (parseFieldsJSON cat =<< o J..: "fields")
  <*> (parseSortJSON cat =<< o J..:? "sort")
  <*> o J..: "count"
  <*> o J..:? "offset" J..!= 0

-------- /api/{catalog}/histogram

defaultHistogramSize, maxHistogramDepth :: Word16
defaultHistogramSize = 16
maxHistogramDepth = 3

histogramSchema :: OpenApiM (OA.Referenced OA.Schema)
histogramSchema = do
  fn <- fieldNameSchema
  define "Histogram" $ mempty & OA.oneOf ?~
    [ fn
    , OA.Inline $ objectSchema "parameters for a single-field histogram (when used as a query parameter, may be specified as \"FIELD:[log]SIZE\"); it's recommended to include fully-bounded range filters for any histogram fields"
      [ ("field", fn, True)
      , ("size", OA.Inline $ schemaDescOf histogramSize "number of buckets to include in the histogram"
        & OA.maximum_ ?~ fromIntegral maxHistogramSize
        & OA.default_ ?~ J.Number (fromIntegral defaultHistogramSize), False)
      , ("log", OA.Inline $ schemaDescOf histogramLog "whether to calculate the histogram using log-spaced buckets (rather than linear spacing)"
        & OA.default_ ?~ J.Bool False, False)
      ]
    ]

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
  f <- lookupField cat True s
  return $ Histogram f defaultHistogramSize False
parseHistogramsJSON cat (J.Object o) = Histogram
  <$> (lookupField cat True =<< o J..: "field")
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
  <*> (mapM (lookupField cat True) =<< o J..:? "quartiles")

-------- global

apiOperations :: [APIOperation]
apiOperations =
  [ APIOperation -- /api
    { apiName = "top"
    , apiSummary = "Get the list of available dataset catalogs"
    , apiPath = R.unit
    , apiExampleArg = ()
    , apiPathParams = []
    , apiAction = \() req -> do
      cats <- asks globalCatalogs
      return $ okResponse (apiHeaders req)
        $ JE.list (J.pairs . catalogJSON) $ filter catalogVisible $ HM.elems $ catalogMap cats
    , apiQueryParams = []
    , apiRequestSchema = Nothing
    , apiResponseSchema = do
      catmeta <- declareSchemaRef (Proxy :: Proxy Catalog)
      return $ mempty
        & OA.type_ ?~ OA.OpenApiArray
        & OA.items ?~ OA.OpenApiItemsObject catmeta
    }

  , APIOperation -- /api/{cat}
    { apiName = "catalog"
    , apiSummary = "Get full metadata about a specific catalog"
    , apiPath = catalogBase
    , apiExampleArg = "sim"
    , apiPathParams = [catalogParam]
    , apiQueryParams = []
    , apiRequestSchema = Nothing
    , apiAction = \sim req -> do
      cat <- askCatalog sim
      (count, stats) <- liftIO $ catalogStats cat
      return $ okResponse (apiHeaders req) $ J.pairs
        $ catalogJSON cat
        <> JE.pair "fields" (fieldsJSON stats mempty $ catalogFieldGroups cat)
        <> "count" J..= fromMaybe count (catalogCount cat)
        <> mwhen (not $ null $ catalogSort cat)
          ("sort" J..= catalogSort cat)
    , apiResponseSchema = do
      fdef <- declareSchemaRef (Proxy :: Proxy FieldGroup)
      meta <- declareSchemaRef (Proxy :: Proxy Catalog)
      return $ mempty & OA.allOf ?~
        [ meta
        , OA.Inline $ objectSchema
          "full catalog metadata"
          [ ("fields", OA.Inline $ arraySchema fdef
            & OA.description ?~ "field groups"
            , True)
          , ("count", OA.Inline $ schemaDescOf catalogCount "total number of rows", True)
          , ("sort", OA.Inline $ schemaDescOf catalogSort "default sort fields", False)
          ]
        ]
    }

  , APIOperation -- /api/{cat}/data
    { apiName = "data"
    , apiSummary = "Get a sample of raw data rows"
    , apiPath = catalogBase R.>* "data"
    , apiExampleArg = "sim"
    , apiPathParams = [catalogParam]
    , apiQueryParams = [filtersQueryParam, fieldsQueryParam
      , sortSchema >>= \sort -> return $ OA.Inline $ mempty
        & OA.name .~ "sort"
        & OA.in_ .~ OA.ParamQuery
        & OA.description ?~ "how to order rows (see descriptions for non-standard formatting of sort order)"
        & OA.style ?~ OA.StyleForm
        & OA.explode ?~ False
        & OA.schema ?~ sort
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
      ]
    , apiRequestSchema = Just $ do
      filt <- declareSchemaRef (Proxy :: Proxy Filters)
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
          ]
        ]
    , apiAction = \sim req -> do
      cat <- askCatalog sim
      body <- parseJSONBody req (parseDataJSON cat)
      let args = fromMaybe (parseDataQuery cat req) body
      unless (dataCount args <= maxDataCount && fromIntegral (dataCount args) + fromIntegral (dataOffset args) <= maxResultWindow)
        $ raise400 "count too large"
      dat <- queryData cat args
      return $ okResponse (apiHeaders req) $ JE.list J.foldable (V.toList dat)
    , apiResponseSchema = do
      fv <- declareSchemaRef (Proxy :: Proxy FieldValue)
      return $ arraySchema $ OA.Inline $ arraySchema fv
        & OA.description ?~ "a single data row corresponding to the requested fields in order (missing values are null)"
    }

  , APIOperation -- /api/{cat}/stats
    { apiName = "stats"
    , apiSummary = "Get statistics about fields (given some filters)"
    , apiPath = catalogBase R.>* "stats"
    , apiExampleArg = "sim"
    , apiPathParams = [catalogParam]
    , apiQueryParams = [filtersQueryParam, fieldsQueryParam]
    , apiRequestSchema = Just $ do
      filt <- declareSchemaRef (Proxy :: Proxy Filters)
      list <- fieldListSchema
      return $ mempty & OA.allOf ?~
        [ filt
        , OA.Inline $ objectSchema
          "stats to request"
          [ ("fields", list, False)
          ]
        ]
    , apiAction = \sim req -> do
      cat <- askCatalog sim
      body <- parseJSONBody req (parseStatsJSON cat)
      (count, stats) <- queryStats cat $ fromMaybe (parseStatsQuery cat req) body
      return $ okResponse (apiHeaders req) $ J.pairs
        $  "count" J..= count
        <> foldMap (\f -> fieldName f `JE.pair` fieldStatsJSON f) stats
    , apiResponseSchema = do
      fs <- declareSchemaRef (Proxy :: Proxy (FieldStats Void))
      return $ objectSchema "stats"
        [ ("count", OA.Inline $ schemaDescOf statsCount "number of matching rows", True) ]
        & OA.additionalProperties ?~ OA.AdditionalPropertiesSchema fs
    }

  , APIOperation -- /api/{cat}/histogram
    { apiName = "histogram"
    , apiSummary = "Get a histogram of data across one or more fields"
    , apiPath = catalogBase R.>* "histogram"
    , apiExampleArg = "sim"
    , apiPathParams = [catalogParam]
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
      filt <- declareSchemaRef (Proxy :: Proxy Filters)
      fn <- fieldNameSchema
      hist <- histogramListSchema
      return $ mempty & OA.allOf ?~
        [ filt
        , OA.Inline $ objectSchema
          "histogram parameters: histogram axes and an optional quartiles axis for which quartiles are calculated in each inner-most histogram bucket"
          [ ("fields", hist, True)
          , ("quartiles", fn, False)
          ]
        ]
    , apiAction = \sim req -> do
      cat <- askCatalog sim
      body <- parseJSONBody req (parseHistogramJSON cat)
      let args = fromMaybe (parseHistogramQuery cat req) body
      unless (length (histogramFields args) + fromEnum (isJust (histogramQuartiles args)) <= fromIntegral maxHistogramDepth
          && product (map (fromIntegral . histogramSize) (histogramFields args)) <= maxHistogramSize)
        $ raise400 "histograms too large"
      HistogramResult{..} <- queryHistogram cat args
      return $ okResponse (apiHeaders req) $ J.pairs
        $  "sizes" J..= histogramSizes
        <> "buckets" `JE.pair` JE.list (\HistogramBucket{..} -> J.pairs
          $  "key" `JE.pair` J.foldable bucketKey
          <> "count" J..= bucketCount
          <> foldMap ("quartiles" J..=) bucketQuartiles)
          histogramBuckets
    , apiResponseSchema = do
      fv <- declareSchemaRef (Proxy :: Proxy FieldValue)
      return $ objectSchema "histogram result"
        [ ("sizes", OA.Inline $ arraySchema (OA.Inline $ schemaDescOf (head . histogramSizes) "size of each histogram buckets in this dimension, either as an absolute width in linear space, or as a ratio in log space"
            & OA.minimum_ ?~ 0
            & OA.exclusiveMinimum ?~ True)
          & OA.description ?~ "field order corresponds to the requested histogram fields and bucket keys", True)
        , ("buckets", OA.Inline $ arraySchema $ OA.Inline $ objectSchema "histogram bucket, representing a box in the dimension space specified by the requested fields"
          [ ("key", OA.Inline $ arraySchema fv
            & OA.description ?~ "the minimum (left) point of this bucket, such than the bucket includes the range [key,key+size) (or [key,key*size) for log scale)", True)
          , ("count", OA.Inline $ schemaDescOf bucketCount "the number of rows with values that fall within this bucket", True)
          , ("quartiles", OA.Inline $ arraySchema fv
            & OA.description ?~ "if quartiles of a field were requested, includes the values of that field corresponding to the [0,25,50,75,100] percentiles ([min, first quartile, median, third quartile, max]) for rows within this bucket", False)
          ], True)
        ]
    }
  ]
  where
  countSchema = OA.Inline $ schemaDescOf dataCount "number of rows to return"
    & OA.maximum_ ?~ fromIntegral maxDataCount
  offsetSchema = OA.Inline $ schemaDescOf dataOffset "start at this row offset (0 means first)"
    & OA.maximum_ ?~ fromIntegral maxResultWindow - fromIntegral maxDataCount
    & OA.default_ ?~ J.Number 0

apiRoutes :: [R.RouteCase Action]
apiRoutes = map (\APIOperation{ apiPath = p, apiAction = a } ->
  R.routeNormCase (apiRouteAction p a)) apiOperations

openApiBase :: OA.OpenApi
openApiBase = mempty &~ do
  OA.info .= (mempty
    & OA.title .~ "FlatHUB API"
    & OA.version .~ T.pack (showVersion Paths.version)
    & OA.description ?~ "Most operations support GET and POST, either of which accepts JSON request bodies or query parameters.  In most cases, query parameters are ignored when there is a request body.")
  OA.servers .= 
    [ OA.Server (requestUrl $ baseApiRequest mempty{ R.requestSecure = True, R.requestHost = RI.splitHost "flathub.flatironinstitute.org" }) (Just "production") mempty
    ]

  mapM_ (\APIOperation{..} -> do
    qparam <- sequence apiQueryParams
    reqs <- sequence apiRequestSchema
    ress <- apiResponseSchema
    let path = foldMap (('/':) . T.unpack) $ pathPlaceholders apiPath apiExampleArg apiPathParams
        pparam = map (\p -> OA.Inline $ p
          & OA.in_ .~ OA.ParamPath
          & OA.required ?~ True) apiPathParams
    op <- (OA.parameters .~ pparam) <$>
      jsonOp apiName apiSummary "" ress
    OA.paths . at' path .= (mempty
      & OA.get ?~ (op
        & OA.parameters %~ (++ qparam))
      & OA.post .~ fmap (\r -> op
        & OA.operationId %~ (fmap (<>"POST"))
        & OA.requestBody ?~ OA.Inline (jsonBody r)) reqs))
    apiOperations

openApi :: Route ()
openApi = getPath "openapi.json" $ \() req ->
  return $ okResponse (apiHeaders req) $ J.encode $ openApiBase
    & OA.servers %~ (OA.Server (requestUrl $ baseApiRequest $ R.waiRequest req) Nothing mempty :)

baseApiRequest :: R.Request -> R.Request
baseApiRequest = RI.requestRoute' (R.routePath apiBase) ()
