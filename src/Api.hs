{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Api
  ( apiTop
  , apiCatalog
  , openApi
  ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict.InsOrd as HMI
import           Data.List (intercalate, stripPrefix)
import           Data.Maybe (fromJust)
import           Data.Monoid (Endo(..))
import qualified Data.OpenApi as OA
import qualified Data.OpenApi.Lens as OAL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Waimwork.Response (response, okResponse)
import qualified Web.Route.Invertible as R
import qualified Web.Route.Invertible.Internal as RI
import qualified Web.Route.Invertible.Render as R
import qualified Web.Route.Invertible.Wai as R

import qualified Paths_flathub as Paths
import Monoid
import Catalog
import Global

catalogJSON :: Catalog -> J.Series
catalogJSON Catalog{..} =
     "name" J..= catalogName
  <> "order" J..= catalogOrder
  <> "title" J..= catalogTitle
  <> "synopsis" J..= catalogSynopsis

apiTop :: Route ()
apiTop = getPath "api" $ \() req -> do
  cats <- asks globalCatalogs
  return $ okResponse [] $ JE.list (J.pairs . catalogJSON) $ filter catalogVisible $ HM.elems $ catalogMap cats

apiCatalog :: Route Simulation
apiCatalog = getPath ("api" R.*< R.parameter) $ \sim req -> do
  cat <- askCatalog sim
  return $ okResponse [] $ J.pairs
    $ catalogJSON cat
    -- <> "fields" J..= catalogFieldGroups cat
    <> foldMap ("count" J..=) (catalogCount cat)
    <> mwhen (not $ null $ catalogSort cat)
      ("sort" J..= catalogSort cat)

openApi :: Route ()
openApi = getPath "openapi.json" $ \() req -> do
  return $ okResponse [] $ J.encode $ mempty
    { OA._openApiInfo = mempty
      { OA._infoTitle = "FlatHUB API"
      , OA._infoVersion = T.pack (show Paths.version)
      }
    , OA._openApiServers = 
      [ OA.Server (urltext $ baseapi $ R.waiRequest req) Nothing mempty
      , OA.Server (urltext $ baseapi produrl) (Just "production") mempty
      ]
    , OA._openApiPaths = HMI.fromList
      [ path apiTop () [] mempty
        { OA._operationSummary = Just ""
        }
      , path apiCatalog "sim" ["sim"] mempty
        { OA._operationParameters = [OA.Inline mempty
          { OA._paramName = "sim"
          }]
        }
      ]
    }
  where
  baseapi = RI.requestRoute' (R.actionRoute apiTop) ()
  produrl = mempty{ R.requestSecure = True, R.requestHost = RI.splitHost "flathub.flatironinstitute.org" }
  urltext r = TE.decodeUtf8 $ BSL.toStrict $ B.toLazyByteString $ R.renderUrlRequestBuilder r mempty
  path r a n x = ('/':rp, Lens.set (mop $ R.requestMethod req) (Just x) mempty) where
    br = baseapi RI.blankRequest
    req = appEndo (RI.foldRoute (\p -> Endo . pp p) (R.actionRoute r) a) br
    pp (RI.RoutePath p) v q = q{ R.requestPath = pvs n (RI.pathValues p v) }
    pp p v q = RI.requestRoutePredicate p v q
    pvs [] [] = []
    pvs nl (RI.PlaceholderValueFixed s:l) = s:pvs nl l
    pvs (n1:nl) (RI.PlaceholderValueParameter _:l) = ('{' `T.cons` n1 `T.snoc` '}'):pvs nl l
    pvs _ _ = error "openApi: parameter name placeholder mismatch"
    rp = intercalate "/" $ map T.unpack $ fromJust $ stripPrefix (R.requestPath br) $ R.requestPath req
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
