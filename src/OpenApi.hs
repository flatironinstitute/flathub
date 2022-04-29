{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module OpenApi
  ( requestUrl
  , routeOperation
  , schemaDescOf
  , objectSchema
  , arraySchema
  , jsonOp
  , zoomDeclare
  , stateDeclareSchema
  , define
  , proxyOf
  ) where

import           Control.Arrow (first)
import           Control.Lens as Lens ((&), Lens', (.~), (?~), over, zoom, At, Index, IxValue, at)
import           Control.Monad.Trans.State (StateT(StateT), State, modify)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict.InsOrd as HMI
import           Data.List (intercalate, stripPrefix)
import           Data.Maybe (fromJust)
import           Data.Monoid (Endo(..))
import qualified Data.OpenApi as OA
import qualified Data.OpenApi.Declare as OAD
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Tuple (swap)
import qualified Web.Route.Invertible as R
import qualified Web.Route.Invertible.Internal as RI
import qualified Web.Route.Invertible.Render as R

import Global

type OpenApiM = State OA.OpenApi
type OpenApi = OpenApiM ()

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

just :: Monoid a => Lens' s (Maybe a) -> Lens' s a
just l f = l (\a -> Just <$> f (fold a))

at' :: (At s, Monoid (IxValue s)) => Index s -> Lens' s (IxValue s)
at' i = just $ at i

class Define a where
  componentsDefinitions :: Lens' OA.Components (OA.Definitions a)

define :: Define a => T.Text -> a -> OpenApiM (OA.Referenced a)
define n x = do
  modify $ OA.components . componentsDefinitions . at n ?~ x
  return $ OA.Ref $ OA.Reference n

instance Define OA.Schema where componentsDefinitions = OA.schemas
instance Define OA.Response where componentsDefinitions = OA.responses
instance Define OA.Param where componentsDefinitions = OA.parameters
instance Define OA.Example where componentsDefinitions = OA.examples
instance Define OA.RequestBody where componentsDefinitions = OA.requestBodies
instance Define OA.Header where componentsDefinitions = OA.headers
instance Define OA.Link where componentsDefinitions = OA.links
instance Define OA.Callback where componentsDefinitions = OA.callbacks

requestUrl :: R.Request -> T.Text
requestUrl r = TE.decodeUtf8 $ BSL.toStrict $ B.toLazyByteString $ R.renderUrlRequestBuilder r mempty

routeOperation :: R.Request -> Route a -> a -> [OA.Param] -> OA.Operation -> OpenApi
routeOperation basereq route arg params op =
  modify $ OA.paths . at' ('/':rp) . mop (R.requestMethod req) ?~ opp
  where
  req = appEndo (RI.foldRoute (\p -> Endo . rrp p) (R.actionRoute route) arg) basereq
  rrp :: RI.RoutePredicate a -> a -> R.Request -> R.Request
  rrp (RI.RoutePath p) v q = q{ R.requestPath = pvs (map OA._paramName params) (RI.pathValues p v) }
  rrp p v q = RI.requestRoutePredicate p v q
  pvs [] [] = []
  pvs nl (RI.PlaceholderValueFixed s:l) = s:pvs nl l
  pvs (n1:nl) (RI.PlaceholderValueParameter _:l) = ('{' `T.cons` n1 `T.snoc` '}'):pvs nl l
  pvs _ _ = error "openApi: parameter name placeholder mismatch"
  rp = intercalate "/" $ map T.unpack $ fromJust $ stripPrefix (R.requestPath basereq) $ R.requestPath req
  mop :: R.Method -> Lens' OA.PathItem (Maybe OA.Operation)
  mop R.GET = OA.get
  mop R.PUT = OA.put
  mop R.POST = OA.post
  mop R.DELETE = OA.delete
  mop R.OPTIONS = OA.options
  mop R.HEAD = OA.head_
  mop R.PATCH = OA.patch
  mop R.TRACE = OA.trace
  mop m = error $ "openApi: unsupported method: " ++ show m
  opp = over OA.parameters (map (\p -> OA.Inline $ p
    & OA.in_ .~ OA.ParamPath
    & OA.required ?~ True
    ) params ++) op

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

schemaDescOf :: OA.ToSchema a => (o -> a) -> T.Text -> OA.Referenced OA.Schema
schemaDescOf f d = OA.Inline $ (OA.toSchema $ proxyOf $ f undefined) & OA.description ?~ d

objectSchema :: T.Text -> [(T.Text, OA.Referenced OA.Schema, Bool)] -> OA.Schema
objectSchema desc props = mempty
  & OA.type_ ?~ OA.OpenApiObject
  & OA.description ?~ desc
  & OA.properties .~ HMI.fromList (map (\(n,s,_) -> (n,s)) props)
  & OA.required .~ map (\(n,_,_) -> n) (filter (\(_,_,r) -> r) props)

arraySchema :: OA.Referenced OA.Schema -> OA.Schema
arraySchema e = mempty
  & OA.type_ ?~ OA.OpenApiArray
  & OA.items ?~ OA.OpenApiItemsObject e

jsonOp :: T.Text -> T.Text -> T.Text -> OA.Schema -> OA.Operation
jsonOp oid summ desc schema = mempty
  & OA.operationId ?~ oid
  & OA.summary ?~ summ
  & OA.responses . OA.responses . at 200 ?~ OA.Inline (mempty
      & OA.description .~ desc
      & OA.content . at' "application/json" . OA.schema ?~ OA.Inline schema)
