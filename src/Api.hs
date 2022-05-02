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

import           Control.Lens ((&), (&~), (.~), (?~), (%~), (^.), over)
import           Control.Monad (mfilter, unless, when, join)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Control.Monad.Trans.State (modify)
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
import           Network.HTTP.Types.Header (ResponseHeaders, hOrigin, hContentType)
import           Network.HTTP.Types.Status (noContent204, badRequest400, requestEntityTooLarge413, unsupportedMediaType415, unprocessableEntity422)
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

decodeUtf8' :: BS.ByteString -> T.Text
decodeUtf8' = TE.decodeUtf8With TE.lenientDecode

eitherBadRequest :: Either String a -> M a
eitherBadRequest = either (result . response badRequest400 []) return

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
    when (x > n) $ result $ response requestEntityTooLarge413 [] ("maximum length " ++ show n)
    b <- Wai.getRequestBodyChunk r
    writeIORef l (x + fromIntegral (BS.length b))
    return b

parseJSONBody :: Wai.Request -> (J.Value -> J.Parser a) -> M (Maybe a)
parseJSONBody req parse = traverse (\c -> do
  unless (c == ct) $ result $ response unsupportedMediaType415 [] $ "expecting " <> ct
  grb <- liftIO $ getRequestBodyChunkLimit 131072 req
  r <- liftIO $ AP.parseWith grb (J.json <* AP.endOfInput) BS.empty
  j <- eitherBadRequest $ AP.eitherResult r
  either (result . response unprocessableEntity422 []) return $ J.parseEither parse j)
  $ lookup hContentType $ Wai.requestHeaders req
  where ct = "application/json"

data APIOperation = forall a . APIOperation
  { apiName :: T.Text
  , apiSummary :: T.Text
  , apiPath :: R.Path a
  , apiExampleArg :: a
  , apiPathParams :: [OA.Param]
  , apiAction :: a -> Action
  , apiParams :: [OpenApiM (OA.Referenced OA.Param)]
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

apiTop :: APIOperation
apiTop = APIOperation
  { apiName = "top"
  , apiSummary = "Get the list of available dataset catalogs"
  , apiPath = R.unit
  , apiExampleArg = ()
  , apiPathParams = []
  , apiAction = \() req -> do
    cats <- asks globalCatalogs
    return $ okResponse (apiHeaders req)
      $ JE.list (J.pairs . catalogJSON) $ filter catalogVisible $ HM.elems $ catalogMap cats
  , apiParams = []
  , apiRequestSchema = Nothing
  , apiResponseSchema = do
    catmeta <- declareSchemaRef (Proxy :: Proxy Catalog)
    return $ mempty
      & OA.type_ ?~ OA.OpenApiArray
      & OA.items ?~ OA.OpenApiItemsObject catmeta
  }

-------- /api/{catalog}

fieldsJSON :: FieldGroup -> FieldGroups -> J.Encoding
fieldsJSON b = JE.list fieldJSON . V.toList where
  fieldJSON f@Field{ fieldName = name, ..} = J.pairs
    $  "key" J..= name
    <> "name" J..= fieldName bf
    <> "title" J..= fieldTitle
    <> "descr" J..= fieldDescr
    <> "type" J..= fieldType
    <> "base" J..= baseType ('f','i','b','s','v') fieldType
    <> foldMap ("enum" J..=) fieldEnum
    <> mwhen (fieldDisp f) ("disp" J..= True)
    <> foldMap ("units" J..=) fieldUnits
    <> foldMap ("required" J..=) (case fieldFlag of
        FieldTop -> Just False
        FieldRequired -> Just True
        _ -> Nothing)
    <> mwhen fieldTerms ("terms" J..= fieldTerms)
    <> mwhen fieldWildcard ("wildcard" J..= fieldWildcard)
    <> foldMap ("dict" J..=) fieldDict
    <> foldMap ("scale" J..=) fieldScale
    <> mwhen fieldReversed ("reversed" J..= fieldReversed)
    <> mwhen (isJust fieldAttachment) ("attachment" J..= True)
    <> foldMap (JE.pair "sub" . fieldsJSON bf) fieldSub
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

apiCatalog :: APIOperation
apiCatalog = APIOperation
  { apiName = "catalog"
  , apiSummary = "Get full metadata about a specific catalog"
  , apiPath = catalogBase
  , apiExampleArg = "sim"
  , apiPathParams = [catalogParam]
  , apiAction = \sim req -> do
    cat <- askCatalog sim
    return $ okResponse (apiHeaders req) $ J.pairs
      $ catalogJSON cat
      <> JE.pair "fields" (fieldsJSON mempty $ catalogFieldGroups cat)
      <> foldMap ("count" J..=) (catalogCount cat)
      <> mwhen (not $ null $ catalogSort cat)
        ("sort" J..= catalogSort cat)
  , apiParams = []
  , apiRequestSchema = Nothing
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
        , ("count", OA.Inline $ schemaDescOf catalogCount "total number of rows (if known)", False)
        , ("sort", OA.Inline $ schemaDescOf catalogSort "default sort fields", False)
        ]
      ]
  }

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
      & OA.anyOf ?~
        [ fv
        , OA.Inline $ arraySchema fv & OA.description ?~ "equal to any of these values (not supported when used as a query parameter)"
          & OA.minItems ?~ 1
          & OA.uniqueItems ?~ True
        , OA.Inline $ objectSchema "in a bounded range for numeric fields: >= gte and <= lte, either of which may be omitted (when used as a query parameter, may be a string containing two FieldValues (one of which may be blank) separated by a single comma or space)"
          [ ("gte", fv, False)
          , ("lte", fv, False)
          ]
          & OA.minProperties ?~ 1
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
    f <- lookupField cat n
    updateFieldValue f <$> traverseTypeValue (parseff f j) (fieldType f)
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
  parseQueryItem q ("sample", Just (rmbs -> Just p)) =
    q{ filterSample = filterSample q * p }
  parseQueryItem q ("sample", Just (spl ('@' ==) -> Just (rmbs -> Just p, rmbs -> Just s))) =
    q{ filterSample = filterSample q * p, filterSeed = Just $ maybe id xor (filterSeed q) s }
  parseQueryItem q (lookupField cat . decodeUtf8' -> Just f, Just (parseFilt f -> Just v)) =
    q{ filterFields = filterFields q <> KM.fromList [updateFieldValue f $ sequenceTypeValue v] }
  parseQueryItem q _ = q -- just ignore anything we can't parse
  parseFilt f (spl isDelim -> Just (a, b))
    | typeIsNumeric (fieldType f) = FieldRange <$> parseVal f a <*> parseVal f b
  parseFilt f a
    | fieldWildcard f && BSC.elem '*' a = return $ FieldWildcard (decodeUtf8' a)
  parseFilt f a = FieldEQ . return <$> parseVal' f a
  parseVal _ "" = return Nothing
  parseVal f v = Just <$> parseVal' f v
  parseVal' f = fmap fieldType . parseFieldValue f . decodeUtf8'
  rmbs :: Read a => BSC.ByteString -> Maybe a
  rmbs = readMaybe . BSC.unpack
  spl c s = (,) p . snd <$> BSC.uncons r
    where (p, r) = BSC.break c s

filtersQueryParam :: OpenApiM (OA.Referenced OA.Param)
filtersQueryParam = do
  filters <- declareSchemaRef (Proxy :: Proxy Filters)
  define "filters" $ mempty
    & OA.name .~ "filters"
    & OA.in_ .~ OA.ParamQuery
    & OA.style ?~ OA.StyleForm
    & OA.explode ?~ True
    & OA.schema ?~ filters
    & OA.description ?~ "filter in query string (see descriptions for non-standard representations of range queries)"

fieldsQueryParam :: OpenApiM (OA.Referenced OA.Param)
fieldsQueryParam = define "fields" $ mempty
  & OA.name .~ "fields"
  & OA.in_ .~ OA.ParamQuery
  & OA.description ?~ "list of fields to return"
  & OA.style ?~ OA.StyleForm
  & OA.explode ?~ False
  & OA.schema ?~ OA.Inline (arraySchema $ OA.Inline $ schemaDescOf T.pack "field name in catalog")

parseFieldsQuery :: Catalog -> Wai.Request -> BS.ByteString -> KM.KeyedMap Field
parseFieldsQuery cat req param =
  KM.fromList $ mapMaybe (lookupField cat . decodeUtf8') $ parseListQuery req param

parseFieldsJSON :: Catalog -> Maybe J.Value -> J.Parser (KM.KeyedMap Field)
parseFieldsJSON _ Nothing = return KM.empty -- hrm
parseFieldsJSON cat (Just j) = KM.fromList <$> (mapM (lookupField cat) =<< J.parseJSON j)

-------- /api/{catalog}/stats

parseStatsQuery :: Catalog -> Wai.Request -> StatsArgs
parseStatsQuery cat req = StatsArgs
  { statsFilters = parseFiltersQuery cat req
  , statsFields = parseFieldsQuery cat req "fields"
  }

parseStatsJSON :: Catalog -> J.Value -> J.Parser StatsArgs
parseStatsJSON cat (J.Object o) = StatsArgs
  <$> parseFiltersJSON cat (HM.delete "fields" o)
  <*> (parseFieldsJSON cat =<< o J..:? "fields")
parseStatsJSON _ j = J.typeMismatch "stats request" j

fieldStatsJSON :: FieldStats -> J.Encoding
fieldStatsJSON FieldStats{..} = J.pairs
  $  "count" J..= statsCount
  <> "min" J..= statsMin
  <> "max" J..= statsMax
  <> "avg" J..= statsAvg
fieldStatsJSON FieldTerms{..} = J.pairs
  $  "terms" `JE.pair` JE.list (\(v,c) -> J.pairs $ "value" J..= v <> "count" J..= c) termsBuckets
  <> "others" J..= termsCount

apiStats :: APIOperation
apiStats = APIOperation
  { apiName = "stats"
  , apiSummary = "Get statistics about fields (given some filters)"
  , apiPath = catalogBase R.>* "stats"
  , apiExampleArg = "sim"
  , apiPathParams = [catalogParam]
  , apiAction = \sim req -> do
    cat <- askCatalog sim
    body <- parseJSONBody req (parseStatsJSON cat)
    (count, stats) <- queryStats cat $ fromMaybe (parseStatsQuery cat req) body
    return $ okResponse (apiHeaders req) $ J.pairs
      $  "count" J..= count
      <> foldMap (\(f, s) -> fieldName f `JE.pair` fieldStatsJSON s) stats
  , apiParams = [filtersQueryParam, fieldsQueryParam]
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
  , apiResponseSchema = do
    fv <- declareSchemaRef (Proxy :: Proxy FieldValue)
    return $ objectSchema "stats"
      [ ("count", OA.Inline $ schemaDescOf statsCount "number of matching rows", True) ]
      & OA.additionalProperties ?~ OA.AdditionalPropertiesSchema (OA.Inline $ mempty
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
        & OA.description ?~ "stats for the field named by the property, depending on its type")
  }

-------- /api/{catalog}/data

parseSortQuery :: Catalog -> Wai.Request -> BS.ByteString -> [(Field, Bool)]
parseSortQuery cat req param =
  mapMaybe parseSort $ parseListQuery req param
  where
  parseSort (BSC.uncons -> Just ('+', lookf -> Just f)) = return (f, True)
  parseSort (BSC.uncons -> Just ('-', lookf -> Just f)) = return (f, False)
  parseSort (lookf -> Just f) = return (f, True)
  parseSort _ = fail "invalid sort"
  lookf = lookupField cat . decodeUtf8'

parseSortJSON :: Catalog -> Maybe J.Value -> J.Parser [(Field, Bool)]
parseSortJSON _ Nothing = return []
parseSortJSON cat (Just j) = J.withArray "sort" (mapM pars . V.toList) j where
  pars :: J.Value -> J.Parser (Field, Bool)
  pars (J.String n) = (, True) <$> lookupField cat n

parseDataQuery :: Catalog -> Wai.Request -> DataArgs
parseDataQuery cat req = DataArgs
  { dataFilters = parseFiltersQuery cat req
  , dataFields = parseFieldsQuery cat req "fields"
  , dataSort = parseSortQuery cat req "sort"
  , dataCount = fromMaybe 0 $ parseReadQuery req "count"
  , dataOffset = fromMaybe 0 $ parseReadQuery req "offset"
  }

parseDataJSON :: Catalog -> J.Value -> J.Parser DataArgs
parseDataJSON cat (J.Object o) = DataArgs
  <$> parseFiltersJSON cat (HM.delete "fields" $ HM.delete "sort" $ HM.delete "count" $ HM.delete "offset" o)
  <*> (parseFieldsJSON cat =<< o J..:? "fields")
  <*> (parseSortJSON cat =<< o J..:? "sort")
  <*> o J..: "count"
  <*> o J..:? "offset" J..!= 0

apiData :: Route (R.Method, Simulation)
apiData = apiRouteAction (catalogBase R.>* "data") $ \sim req -> do
  cat <- askCatalog sim
  body <- parseJSONBody req (parseDataJSON cat)
  dat <- queryData cat $ fromMaybe (parseDataQuery cat req) body
  return $ okResponse (apiHeaders req) $ J.pairs
    mempty
--        "Get a sample of raw data rows"

-------- /openapi.json

apiOperations :: [APIOperation]
apiOperations =
  [ apiTop
  , apiCatalog
  , apiStats
  -- , apiData
  ]

apiRoutes :: [R.RouteCase Action]
apiRoutes = map (\APIOperation{ apiPath = p, apiAction = a } ->
  R.routeNormCase (apiRouteAction p a)) apiOperations

openApi :: Route ()
openApi = getPath "openapi.json" $ \() req ->
  return $ okResponse (apiHeaders req) $ J.encode $ mempty &~ do
    modify
      $ (over OA.info
        $ (OA.title .~ "FlatHUB API")
        . (OA.version .~ T.pack (showVersion Paths.version))
        . (OA.description ?~ "Most operations support GET and POST, either of which accepts JSON request bodies or query parameters.  In most cases, query parameters are ignored when there is a request body."))
      . (OA.servers .~
        [ OA.Server (urltext $ baseapi $ R.waiRequest req) Nothing mempty
        , OA.Server (urltext $ baseapi produrl) (Just "production") mempty
        ])

    mapM_ (\APIOperation{..} -> do
      qparam <- sequence apiParams
      reqs <- sequence apiRequestSchema
      ress <- apiResponseSchema
      path (apiRouteAction apiPath apiAction) apiExampleArg apiPathParams
        . (OA.parameters .~ qparam)
        . (OA.requestBody .~ fmap (OA.Inline . jsonBody) reqs)
        =<< jsonOp apiName apiSummary "" ress)
      apiOperations
  where
  baseapi = RI.requestRoute' (R.routePath apiBase) ()
  produrl = mempty{ R.requestSecure = True, R.requestHost = RI.splitHost "flathub.flatironinstitute.org" }
  urltext = requestUrl
  path a x p o = do
    pathop a (R.GET, x) p (o
      & OA.requestBody .~ Nothing)
    when (isJust $ o ^. OA.requestBody) $
      pathop a (R.POST, x) p (o
        & OA.operationId %~ fmap (<>"POST")
        & OA.parameters %~ filter (not . isqp))
  pathop = routeOperation (baseapi RI.blankRequest)
  isqp (OA.Inline (OA.Param{ OA._paramIn = OA.ParamQuery })) = True
  isqp _ = False
