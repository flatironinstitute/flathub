{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module OpenApi
  ( requestUrl
  , routeOperation
  , schemaDescOf
  , objectSchema
  , arraySchema
  , jsonOp
  ) where

import qualified Control.Lens as Lens
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict.InsOrd as HMI
import           Data.List (intercalate, stripPrefix)
import           Data.Maybe (fromJust)
import           Data.Monoid (Endo(..))
import qualified Data.OpenApi as OA
import qualified Data.OpenApi.Lens as OAL
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Web.Route.Invertible as R
import qualified Web.Route.Invertible.Internal as RI
import qualified Web.Route.Invertible.Render as R

import Global

requestUrl :: R.Request -> T.Text
requestUrl r = TE.decodeUtf8 $ BSL.toStrict $ B.toLazyByteString $ R.renderUrlRequestBuilder r mempty

routeOperation :: R.Request -> Route a -> a -> [OA.Param] -> OA.Operation -> (String, OA.PathItem)
routeOperation basereq route arg params op = ('/':rp, Lens.set (mop $ R.requestMethod req) (Just opp) mempty) where
  req = appEndo (RI.foldRoute (\p -> Endo . rrp p) (R.actionRoute route) arg) basereq
  rrp (RI.RoutePath p) v q = q{ R.requestPath = pvs (map OA._paramName params) (RI.pathValues p v) }
  rrp p v q = RI.requestRoutePredicate p v q
  pvs [] [] = []
  pvs nl (RI.PlaceholderValueFixed s:l) = s:pvs nl l
  pvs (n1:nl) (RI.PlaceholderValueParameter _:l) = ('{' `T.cons` n1 `T.snoc` '}'):pvs nl l
  pvs _ _ = error "openApi: parameter name placeholder mismatch"
  rp = intercalate "/" $ map T.unpack $ fromJust $ stripPrefix (R.requestPath basereq) $ R.requestPath req
  mop :: R.Method -> Lens.Lens' OA.PathItem (Maybe OA.Operation)
  mop R.GET = OAL.get
  mop R.PUT = OAL.put
  mop R.POST = OAL.post
  mop R.DELETE = OAL.delete
  mop R.OPTIONS = OAL.options
  mop R.HEAD = OAL.head_
  mop R.PATCH = OAL.patch
  mop R.TRACE = OAL.trace
  mop m = error $ "openApi: unsupported method: " ++ show m
  opp = Lens.over OAL.parameters (map (\p -> OA.Inline p
    { OA._paramIn = OA.ParamPath
    , OA._paramRequired = Just True
    }) params ++) op

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

schemaDescOf :: OA.ToSchema a => (o -> a) -> T.Text -> OA.Referenced OA.Schema
schemaDescOf f d = OA.Inline $ (OA.toSchema $ proxyOf $ f undefined){ OA._schemaDescription = Just d }

objectSchema :: T.Text -> [(T.Text, OA.Referenced OA.Schema, Bool)] -> OA.Schema
objectSchema desc props = mempty
  { OA._schemaType = Just OA.OpenApiObject
  , OA._schemaDescription = Just desc
  , OA._schemaProperties = HMI.fromList $ map (\(n,s,_) -> (n,s)) props
  , OA._schemaRequired = map (\(n,_,_) -> n) $ filter (\(_,_,r) -> r) props
  }

arraySchema :: OA.Referenced OA.Schema -> OA.Schema
arraySchema e = mempty
  { OA._schemaType = Just OA.OpenApiArray
  , OA._schemaItems = Just $ OA.OpenApiItemsObject e
  }

jsonOp :: T.Text -> T.Text -> T.Text -> OA.Schema -> OA.Operation
jsonOp oid summ desc schema = mempty
  { OA._operationOperationId = Just oid
  , OA._operationSummary = Just summ
  , OA._operationResponses = mempty
    { OA._responsesResponses = HMI.singleton 200 $ OA.Inline mempty
      { OA._responseDescription = desc
      , OA._responseContent = HMI.singleton "application/json" mempty
        { OA._mediaTypeObjectSchema = Just $ OA.Inline schema
        }
      }
    }
  }
