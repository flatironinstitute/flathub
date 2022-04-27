{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Api
  ( apiTop
  , apiCatalog
  , openApi
  ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict.InsOrd as HMI
import           Data.Maybe (fromJust, isJust)
import qualified Data.OpenApi as OA
import qualified Data.OpenApi.Declare as OAD
import qualified Data.OpenApi.Lens as OAL
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Version (showVersion)
import           Waimwork.Response (response, okResponse)
import qualified Web.Route.Invertible as R
import qualified Web.Route.Invertible.Internal as RI
import qualified Web.Route.Invertible.Wai as R

import qualified Paths_flathub as Paths
import Monoid
import Type
import Field
import Catalog
import Global
import OpenApi

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
      [ ("name", schemaDescOf catalogName "globally unique catalog id used in urls", True)
      , ("order", schemaDescOf catalogOrder "sort key for display order", True)
      , ("title", schemaDescOf catalogTitle "display name", True)
      , ("synopsis", schemaDescOf catalogSynopsis "short description", True)
      ]

apiTop :: Route ()
apiTop = getPath "api" $ \() _ -> do
  cats <- asks globalCatalogs
  return $ okResponse [] $ JE.list (J.pairs . catalogJSON) $ filter catalogVisible $ HM.elems $ catalogMap cats

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
    <> "disp" J..= fieldDisp f
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
      { OA._schemaDescription = Just "storage type"
      , OA._schemaType = Just OA.OpenApiString
      , OA._schemaEnum = Just $ map J.toJSON [Keyword Proxy, Long Proxy, Integer Proxy, Short Proxy, Byte Proxy, Double Proxy, Float Proxy, HalfFloat Proxy, Boolean Proxy, Void Proxy]
      }

instance OA.ToSchema FieldGroup where
  declareNamedSchema t = do
    ref <- OA.declareSchemaRef t
    return $ OA.NamedSchema (Just "FieldGroup") $ objectSchema
      "A single field within a catalog, or a hiearchical group of fields"
      [ ("key", schemaDescOf fieldName "local name of field within this group", True)
      , ("name", schemaDescOf fieldName "global unique (\"variable\") name of field within the catalog", True)
      , ("title", schemaDescOf fieldTitle "display name of the field within the group", True)
      , ("descr", schemaDescOf (fromJust . fieldDescr) "description of field within the group", False)
      , ("type", schemaDescOf typeOfValue "raw storage type", True)
      , ("base", OA.Inline mempty
          { OA._schemaDescription = Just "base storage type (floating, integral, boolean, string, void)"
          , OA._schemaType = Just OA.OpenApiString
          , OA._schemaEnum = Just $ map J.toJSON "fibsv"
          }, True)
      , ("enum", schemaDescOf (fromJust . fieldEnum) "if present, display values as these keywords instead (integral or boolean: enum[<int>value])", False)
      , ("disp", schemaDescOf fieldDisp "include field in data display by default", True)
      , ("units", schemaDescOf (fromJust . fieldUnits) "display units", False)
      , ("required", schemaDescOf (True ==) "true = required filter; false = top-level (default) optional filter; missing = normal", False)
      , ("terms", schemaDescOf fieldTerms "display dynamically as a dropdown of values", False)
      , ("dict", schemaDescOf (fromJust . fieldDict) "unique key index to global field dictionary (for compare)", False)
      , ("scale", schemaDescOf (fromJust . fieldScale) "scale factor to dict-comparable units, display  value*scale (for compare)", False)
      , ("reversed", schemaDescOf fieldReversed "display axes and ranges in reverse (high-low)", False)
      , ("attachment", schemaDescOf isJust "this is a meta field for a downloadable attachment (type boolean, indicating presence)", False)
      , ("wildcard", schemaDescOf fieldWildcard "allow wildcard prefix searching on keyword field (\"xy*\")", False)
      , ("sub", OA.Inline (arraySchema ref)
          { OA._schemaDescription = Just "child fields: if this is present, this is a pseudo grouping field which does not exist itself, but its properties apply to its children"
          }, False)
      ]

catalogSchema :: OAD.Declare (OA.Definitions OA.Schema) OA.Schema
catalogSchema = do
  fdef <- OA.declareSchemaRef (Proxy :: Proxy FieldGroup)
  return $ objectSchema
    "Full catalog metadata"
    [ ("fields", OA.Inline (arraySchema fdef)
      { OA._schemaDescription = Just "field groups"
      }, True)
    , ("count", schemaDescOf (fromJust . catalogCount) "total number of rows (if known)", False)
    , ("sort", schemaDescOf catalogSort "default sort fields", False)
    ]

apiCatalog :: Route Simulation
apiCatalog = getPath ("api" R.*< R.parameter) $ \sim _ -> do
  cat <- askCatalog sim
  return $ okResponse [] $ J.pairs
    $ catalogJSON cat
    <> JE.pair "fields" (fieldsJSON mempty $ catalogFieldGroups cat)
    <> foldMap ("count" J..=) (catalogCount cat)
    <> mwhen (not $ null $ catalogSort cat)
      ("sort" J..= catalogSort cat)

openApi :: Route ()
openApi = getPath "openapi.json" $ \() req -> do
  let
    (defs, spec) = OAD.runDeclare decl mempty
    decl = do
      catmeta <- OA.declareSchemaRef (Proxy :: Proxy Catalog)
      catschema <- catalogSchema
      return mempty
        { OA._openApiInfo = mempty
          { OA._infoTitle = "FlatHUB API"
          , OA._infoVersion = T.pack (showVersion Paths.version)
          }
        , OA._openApiServers = 
          [ OA.Server (urltext $ baseapi $ R.waiRequest req) Nothing mempty
          , OA.Server (urltext $ baseapi produrl) (Just "production") mempty
          ]
        , OA._openApiComponents = mempty
          { OA._componentsSchemas = defs
          }
        , OA._openApiPaths = HMI.fromList
          [ path apiTop () [] $ jsonOp
            "Get the list of available dataset catalogs"
            "list of catalogs"
            mempty
            { OA._schemaType = Just OA.OpenApiArray
            , OA._schemaItems = Just $ OA.OpenApiItemsObject catmeta
            }
          , path apiCatalog "sim" 
            [ mempty
              { OA._paramName = "sim"
              , OA._paramSchema = Just $ schemaDescOf T.pack "catalog id"
              }
            ] $ jsonOp
            "Get full metadata about a specific catalog"
            "catalog metadata"
            mempty
            { OA._schemaAllOf = Just
              [ catmeta
              , OA.Inline catschema
              ]
            }
          ]
        }
  return $ okResponse [] $ J.encode $ spec
  where
  baseapi = RI.requestRoute' (R.actionRoute apiTop) ()
  produrl = mempty{ R.requestSecure = True, R.requestHost = RI.splitHost "flathub.flatironinstitute.org" }
  urltext = requestUrl
  path = routeOperation (baseapi RI.blankRequest)
