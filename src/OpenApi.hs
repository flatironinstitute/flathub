{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module OpenApi
  ( routeRequestUrl
  , pathPlaceholders
  , OpenApiM
  , stateDeclareSchema
  , declareSchemaRef
  , define
  , defineRec
  , resolve
  , schemaDescOf
  , objectSchema
  , arraySchema
  , jsonContent
  , makeOp
  , zoomDeclare
  , at'
  ) where

import           Control.Arrow (first)
import           Control.Lens as Lens ((&), Lens', (.~), (?~), zoom, At, Index, IxValue, at, (.=), use)
import           Control.Monad.Trans.State (StateT(StateT), State)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict.InsOrd as HMI
import           Data.Maybe (fromMaybe)
import qualified Data.OpenApi as OA
import qualified Data.OpenApi.Declare as OAD
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Tuple (swap)
import           Network.HTTP.Media.MediaType (MediaType)
import qualified Web.Route.Invertible as R
import qualified Web.Route.Invertible.Internal as RI
import qualified Web.Route.Invertible.Render as R

type OpenApiM = State OA.OpenApi

newtype Focusing m a d = Focusing{ unfocusing :: m (d, a) }
instance Functor m => Functor (Focusing m a) where
  fmap f (Focusing x) = Focusing $ fmap (first f) x

zoomDeclare :: Functor m => Lens' d e -> OAD.DeclareT e m a -> OAD.DeclareT d m a
zoomDeclare l f = OAD.DeclareT $ unfocusing <$> l (Focusing <$> OAD.runDeclareT f)

-- |Lift 'OAD.DeclareT' into 'StateT'
stateDeclare :: (Functor m, Semigroup s) => OAD.DeclareT s m a -> StateT s m a
stateDeclare f = StateT $ \s -> swap . first (s <>) <$> OAD.runDeclareT f s

stateDeclareSchema :: (Monad m, Show a) => OAD.DeclareT (OA.Definitions OA.Schema) m a -> StateT OA.OpenApi m a
stateDeclareSchema = zoom (OA.components . OA.schemas) . stateDeclare

declareSchemaRef :: OA.ToSchema a => Proxy a -> OpenApiM (OA.Referenced OA.Schema)
declareSchemaRef = stateDeclareSchema . OA.declareSchemaRef

just :: Monoid a => Lens' s (Maybe a) -> Lens' s a
just l f = l (\a -> Just <$> f (fold a))

at' :: (At s, Monoid (IxValue s)) => Index s -> Lens' s (IxValue s)
at' i = just $ at i

class Define a where
  componentsDefinitions :: Lens' OA.Components (OA.Definitions a)

define :: Define a => T.Text -> a -> OpenApiM (OA.Referenced a)
define n x = do
  OA.components . componentsDefinitions . at n .= Just x
  return $ OA.Ref $ OA.Reference n

defineRec :: Define a => T.Text -> (OA.Referenced a -> a) -> OpenApiM (OA.Referenced a)
defineRec n f = define n $ f $ OA.Ref $ OA.Reference n

resolve :: Define a => OA.Referenced a -> OpenApiM a
resolve (OA.Inline x) = return x
resolve (OA.Ref (OA.Reference n)) = fromMaybe (error $ "OpenApi.resolve undefined: " ++ show n)
  <$> use (OA.components . componentsDefinitions . at n)

instance Define OA.Schema where componentsDefinitions = OA.schemas
instance Define OA.Response where componentsDefinitions = OA.responses
instance Define OA.Param where componentsDefinitions = OA.parameters
instance Define OA.Example where componentsDefinitions = OA.examples
instance Define OA.RequestBody where componentsDefinitions = OA.requestBodies
instance Define OA.Header where componentsDefinitions = OA.headers
instance Define OA.Link where componentsDefinitions = OA.links
instance Define OA.Callback where componentsDefinitions = OA.callbacks

routeRequestUrl :: R.Request -> T.Text
routeRequestUrl r = TE.decodeUtf8 $ BSL.toStrict $ B.toLazyByteString $ R.renderUrlRequestBuilder r mempty

pathPlaceholders :: R.Path a -> a -> [OA.Param] -> [T.Text]
pathPlaceholders path arg params =
  pvs (map OA._paramName params) (RI.pathValues path arg)
  where
  pvs [] [] = []
  pvs nl (RI.PlaceholderValueFixed s:l) = s:pvs nl l
  pvs (n1:nl) (RI.PlaceholderValueParameter _:l) = ('{' `T.cons` n1 `T.snoc` '}'):pvs nl l
  pvs _ _ = error "openApi: parameter name placeholder mismatch"

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

schemaDescOf :: OA.ToSchema a => (o -> a) -> T.Text -> OA.Schema
schemaDescOf f d = (OA.toSchema $ proxyOf $ f undefined) & OA.description ?~ d

objectSchema :: T.Text -> [(T.Text, OA.Referenced OA.Schema, Bool)] -> OA.Schema
objectSchema desc props = mempty
  & OA.type_ ?~ OA.OpenApiObject
  & OA.description ?~ desc
  & OA.properties .~ HMI.fromList (map (\(n,s,_) -> (n,s)) props)
  & OA.additionalProperties ?~ OA.AdditionalPropertiesAllowed False
  & OA.required .~ map (\(n,_,_) -> n) (filter (\(_,_,r) -> r) props)

arraySchema :: OA.Referenced OA.Schema -> OA.Schema
arraySchema e = mempty
  & OA.type_ ?~ OA.OpenApiArray
  & OA.items ?~ OA.OpenApiItemsObject e

type ContentMap = HMI.InsOrdHashMap MediaType OA.MediaTypeObject

jsonContent :: (Monoid a, OA.HasContent a ContentMap) => OA.Referenced OA.Schema -> a
jsonContent schema = mempty
  & OA.content . at' "application/json" . OA.schema ?~ schema

makeOp :: T.Text -> T.Text -> OA.Response -> OpenApiM OA.Operation
makeOp oid summ res = do
  resp <- define oid res
  return $ mempty
    & OA.operationId ?~ oid
    & OA.summary ?~ summ
    & OA.responses . OA.responses . at 200 ?~ resp
