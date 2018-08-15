{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Html
  ( top
  , simulation
  , staticHtml
  ) where

import           Control.Monad (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import           Data.List (find)
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import           Network.HTTP.Types.Header (ResponseHeaders, hAccept, hIfModifiedSince, hLastModified, hContentType, hCacheControl)
import           Network.HTTP.Types.Status (notModified304, notFound404)
import qualified Network.Wai as Wai
import           Network.Wai.Parse (parseHttpAccept)
import qualified System.FilePath as FP
import qualified Text.Blaze.Html5 as H hiding (text, textValue)
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Waimwork.Blaze as H (text, textValue, preEscapedBuilder)
import qualified Waimwork.Blaze as WH
import           Waimwork.HTTP (parseHTTPDate, formatHTTPDate)
import           Waimwork.Response (response, okResponse)
import           Waimwork.Result (result)
import qualified Web.Route.Invertible as R
import           Web.Route.Invertible.URI (routeActionURI)

import Field
import Catalog
import Global
import Compression
import Query
import Static

htmlResponse :: Wai.Request -> ResponseHeaders -> H.Markup -> M Wai.Response
htmlResponse req hdrs body = do
  cats <- asks $ catalogMap . globalCatalogs
  return $ okResponse hdrs $ H.docTypeHtml $ do
    H.head $ do
      forM_ ([["jspm_packages", if isdev then "system.src.js" else "system.js"], ["jspm.config.js"]] ++ if isdev then [["dev.js"]] else [["index.js"]]) $ \src ->
        H.script H.! HA.type_ "text/javascript" H.! HA.src (staticURI src) $ mempty
      -- TODO: use System.resolve:
      forM_ [["jspm_packages", "npm", "datatables.net-dt@1.10.19", "css", "jquery.dataTables.css"], ["main.css"]] $ \src ->
        H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href (staticURI src)
      H.script H.! HA.type_ "text/javascript" H.! HA.src "//cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3/MathJax.js?config=TeX-AMS_CHTML" $ mempty
      H.script $ do
        "Catalogs="
        H.unsafeLazyByteString $ J.encode $ HM.map catalogTitle cats
    H.body $ do
      body
      H.footer $ do
        H.a H.! HA.href "https://github.com/flatironinstitute/astrosims-reproto" $
          H.img H.! HA.src (staticURI ["github.png"])
  where
  isdev = any ((==) "dev" . fst) $ Wai.queryString req

acceptable :: [BS.ByteString] -> Wai.Request -> Maybe BS.ByteString
acceptable l = find (`elem` l) . foldMap parseHttpAccept . lookup hAccept . Wai.requestHeaders

top :: Route ()
top = getPath R.unit $ \() req -> do
  cats <- asks (catalogMap . globalCatalogs)
  case acceptable ["application/json", "text/html"] req of
    Just "application/json" ->
      return $ okResponse [] $ J.encode $ HM.map catalogTitle cats
    _ -> htmlResponse req [] $
      H.dl $
        forM_ (HM.toList cats) $ \(sim, cat) -> do
          H.dt $ H.a H.! HA.href (WH.routeActionValue simulation sim mempty) $
            H.text $ catalogTitle cat
          mapM_ (H.dd . H.preEscapedText) $ catalogDescr cat

simulation :: Route Simulation
simulation = getPath R.parameter $ \sim req -> do
  cat <- askCatalog sim
  let 
    (_, quri) = routeActionURI simulation sim
    fields = catalogFieldGroups cat
    fields' = catalogFields cat
    jcat = J.pairs $
         "title" J..= catalogTitle cat
      <> "descr" J..= catalogDescr cat
      <> "uri" J..= show quri
      <> "bulk" J..= map (J.String . R.renderParameter) [BulkCSV Nothing, BulkCSV (Just CompressionGZip), BulkNumpy Nothing, BulkNumpy (Just CompressionGZip)]
      <> "fields" J..= fields'
    fieldBody :: Word -> FieldGroup -> H.Html
    fieldBody d f = H.span WH.!? (HA.title . H.textValue <$> fieldDescr f) $ do
      H.text $ fieldTitle f
      forM_ (fieldUnits f) $ \u -> do
        if d > 1 then H.br else " "
        H.span H.! HA.class_ "units" $ "[" <> H.preEscapedText u <> "]"
    field :: Word -> FieldGroup -> FieldGroup -> H.Html
    field d f' f@Field{ fieldSub = Nothing } = do
      H.th
          H.! HA.rowspan (H.toValue d)
          H.! H.dataAttribute "data" (H.textValue $ fieldName f')
          H.! H.dataAttribute "name" (H.textValue $ fieldName f')
          H.! H.dataAttribute "type" (dtype $ fieldType f)
          H.!? (not (fieldDisp f), H.dataAttribute "visible" "false")
          H.! H.dataAttribute "default-content" mempty $ do
        H.span
          H.! HA.id ("hide-" <> H.textValue (fieldName f')) H.! HA.class_ "hide"
          H.! HA.onclick "return hide_column(event)"
          $ H.preEscapedString "&times;"
        fieldBody d f
    field _ _ f@Field{ fieldSub = Just s } = do
      H.th
          H.! HA.colspan (H.toValue $ length $ expandFields s) $
        fieldBody 1 f
    row :: Word -> [(FieldGroup -> FieldGroup, FieldGroup)] -> H.Html
    row d l = do
      H.tr $ mapM_ (\(p, f) -> field d (p f) f) l
      when (d > 1) $ row (pred d) $ foldMap (\(p, f) -> foldMap (fmap (p . mappend f, ) . V.toList) $ fieldSub f) l
    query = parseQuery req
  case acceptable ["application/json", "text/html"] req of
    Just "application/json" ->
      return $ okResponse [] jcat
    _ -> htmlResponse req [] $ do
      H.script $ do
        "Catalog="
        H.preEscapedBuilder $ J.fromEncoding jcat
        ";Query="
        H.unsafeLazyByteString $ J.encode query
        ";"
      H.h2 $ H.text $ catalogTitle cat
      mapM_ (H.p . H.preEscapedText) $ catalogDescr cat
      H.p $ "Query and explore a subset using the filters, download your selection using the link below, or get the full dataset above."
      H.table H.! HA.id "filt" $ mempty
      H.div H.! HA.id "dhist" $ do
        forM_ ['x','y'] $ \xy -> let xyv = H.stringValue [xy] in
          H.div H.! HA.id ("dhist-" <> xyv) H.! HA.class_ "dhist-xy" $
            H.button H.! HA.id ("dhist-" <> xyv <> "-tog") H.! HA.class_ "dhist-xy-tog" $
              "lin/log"
        H.canvas H.! HA.id "hist" $ mempty
      H.table H.! HA.id "tcat" H.! HA.class_ "compact" $ do
        H.thead $ row (fieldsDepth fields) ((id ,) <$> V.toList fields)
        H.tfoot $ H.tr $ H.td H.! HA.colspan (H.toValue $ length fields') H.! HA.class_ "loading" $ "loading..."
  where
  dtype (Long _) = "num"
  dtype (Integer _) = "num"
  dtype (Short _) = "num"
  dtype (Byte _) = "num"
  dtype (Double _) = "num"
  dtype (Float _) = "num"
  dtype (HalfFloat _) = "num"
  -- dtype (Date _) = "date"
  dtype _ = "string"

staticHtml :: Route [FilePathComponent]
staticHtml = getPath ("html" R.*< R.manyI R.parameter) $ \paths q -> do
  let path = FP.joinPath ("html" : map componentFilePath paths) FP.<.> "html"
  fmod <- maybe (result $ response notFound404 [] (path ++ " not found")) return =<<
    liftIO (getModificationTime' path)
  if any (fmod <=) $ parseHTTPDate =<< lookup hIfModifiedSince (Wai.requestHeaders q)
    then return $ Wai.responseBuilder notModified304 [] mempty
    else do
      bod <- liftIO $ BSL.readFile path
      htmlResponse q
        [ (hLastModified, formatHTTPDate fmod)
        , (hContentType, "text/html")
        , (hCacheControl, "public, max-age=3600")
        ] (H.unsafeLazyByteString bod)

