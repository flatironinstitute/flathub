{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Html
  ( topPage
  , simulationPage
  , sqlSchema
  , comparePage
  , staticHtml
  ) where

import           Control.Monad (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import           Data.List (find, sortOn)
import           Data.Maybe (isNothing)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.HTTP.Types.Header (ResponseHeaders, hAccept, hIfModifiedSince, hLastModified, hContentType)
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

import Field
import Catalog
import Global
import Compression
import Query
import Monoid
import Static

jsonEncodingVar :: T.Text -> J.Encoding -> H.Html
jsonEncodingVar var enc = H.script $ do
  H.text var
  "="
  H.preEscapedBuilder $ J.fromEncoding enc
  ";"

jsonVar :: J.ToJSON a => T.Text -> a -> H.Html
jsonVar var = jsonEncodingVar var . J.toEncoding

catalogsSorted :: Catalogs -> [(T.Text, Catalog)]
catalogsSorted = sortOn (catalogSort . snd) . HM.toList . catalogMap

htmlResponse :: Wai.Request -> ResponseHeaders -> H.Markup -> M Wai.Response
htmlResponse req hdrs body = do
  glob <- ask
  let isdev = globalDevMode glob && any ((==) "dev" . fst) (Wai.queryString req)
  return $ okResponse hdrs $ H.docTypeHtml $ do
    H.head $ do
      forM_ ([["jspm_packages", if isdev then "system.src.js" else "system.js"], ["jspm.config.js"]] ++ if isdev then [["dev.js"]] else [["index.js"]]) $ \src ->
        H.script H.! HA.type_ "text/javascript" H.! HA.src (staticURI src) $ mempty
      -- TODO: use System.resolve:
      forM_ [["jspm_packages", "npm", "datatables.net-dt@1.10.19", "css", "jquery.dataTables.css"], ["main.css"]] $ \src ->
        H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href (staticURI src)
      H.script H.! HA.type_ "text/javascript" H.! HA.src "//cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_CHTML" $ mempty
      jsonVar "Catalogs" (HM.map catalogTitle $ catalogMap $ globalCatalogs glob)
    H.body $ do
      H.div H.! HA.id "bar" $ do
          H.ul H.! HA.id "topbar" $ do
              H.li $ do
                  H.a H.! HA.href "https://www.simonsfoundation.org/flatiron/" H.! HA.id "flaticon" $
                      H.img H.! HA.src (staticURI ["flatiron.svg"]) H.! HA.height "40" H.! HA.width "40"
              H.li $
                H.a H.! HA.href (WH.routeActionValue topPage () mempty) $ H.text "Home"
              H.li $
                H.div H.! HA.class_ "dropdown" $ do
                    H.a H.! HA.href (WH.routeActionValue topPage () mempty) $ H.text "Catalogs"
                    H.div H.! HA.class_ "dropdown-content" $ do
                        forM_ (catalogsSorted $ globalCatalogs glob) $ \(key, cat) ->
                            H.a H.! HA.href (WH.routeActionValue simulationPage key mempty) $ H.text (catalogTitle cat)
              H.li $
                H.a H.! HA.href (WH.routeActionValue comparePage () mempty) $ H.text "Compare"
              H.li $
                H.a H.! HA.href (WH.routeActionValue staticHtml ["candels"] mempty) $ H.text "CANDELS"
      body
      H.footer $ do
        H.a H.! HA.href "https://github.com/flatironinstitute/astrosims-reproto" $
            H.img H.! HA.src (staticURI ["github.png"])

acceptable :: [BS.ByteString] -> Wai.Request -> Maybe BS.ByteString
acceptable l = find (`elem` l) . foldMap parseHttpAccept . lookup hAccept . Wai.requestHeaders

-- Here is where the main page is generated
topPage :: Route ()
topPage = getPath R.unit $ \() req -> do
  cats <- asks globalCatalogs
  case acceptable ["application/json", "text/html"] req of
    Just "application/json" ->
      return $ okResponse [] $ J.encode $ HM.map catalogTitle $ catalogMap cats
    _ -> htmlResponse req [] $
      H.dl $
        forM_ (catalogsSorted cats) $ \(sim, cat) -> do
          H.dt $ H.a H.! HA.href (WH.routeActionValue simulationPage sim mempty) $
            H.text $ catalogTitle cat
          H.dd $ do
            mapM_ H.preEscapedText $ catalogDescr cat
            mapM_ ((" " <>) . (<> " rows.") . H.toMarkup) $ catalogCount cat

simulationPage :: Route Simulation
simulationPage = getPath R.parameter $ \sim req -> do
  cat <- askCatalog sim
  let
    fields = catalogFieldGroups cat
    fields' = catalogFields cat
    jcat = J.pairs $
         "name" J..= sim
      <> "title" J..= catalogTitle cat
      <> "descr" J..= catalogDescr cat
      <> "bulk" J..= map (J.String . R.renderParameter) [BulkCSV Nothing, BulkCSV (Just CompressionGZip), BulkNumpy Nothing, BulkNumpy (Just CompressionGZip)]
      <> "fields" J..= fields'
    fieldBody :: Word -> FieldGroup -> H.Html
    fieldBody d f = H.span $ do
      -- Writes the title to the span
      H.text $ fieldTitle f
      -- Writes the unit to the span
      forM_ (fieldUnits f) $ \u -> do
        when (d > 1) H.br
        H.span H.! HA.class_ "units" $ H.preEscapedText u
      mapM_ ((H.span H.! HA.class_ "tooltiptext") . H.text) (fieldDescr f)
    field :: Word -> FieldGroup -> FieldGroup -> H.Html
    field d f' f@Field{ fieldSub = Nothing } = do
      H.th
          H.! HA.rowspan (H.toValue d)
          H.! H.dataAttribute "data" (H.textValue $ fieldName f')
          H.! H.dataAttribute "name" (H.textValue $ fieldName f')
          H.! H.dataAttribute "type" (baseType ("num","num","string","string") $ fieldType f)
          H.!? (not (fieldDisp f), H.dataAttribute "visible" "false")
          H.! H.dataAttribute "default-content" mempty
          H.! HA.class_ "tooltip" $
        fieldBody d f
    field _ _ f@Field{ fieldSub = Just s } = do
      H.th
          H.! HA.colspan (H.toValue $ length $ expandFields s)
          H.! HA.class_ "tooltip" $
        fieldBody 1 f
    row :: Word -> [(FieldGroup -> FieldGroup, FieldGroup)] -> H.Html
    row d l = do
      H.tr $ mapM_ (\(p, f) -> field d (p f) f) l
      when (d > 1) $ row (pred d) $ foldMap (\(p, f) -> foldMap (fmap (p . mappend f, ) . V.toList) $ fieldSub f) l
    query = parseQuery cat req

    fielddesc :: FieldGroup -> FieldGroup -> Int -> H.Html
    fielddesc f g d = do
        H.tr $ do
            H.td H.! HA.class_ ("depth-" <> H.stringValue (show d)) $ do
                H.input H.! HA.type_ "checkbox"
                    H.!? (isNothing (fieldSub g), HA.id $ H.textValue $ key f)
                    H.! HA.class_ (H.textValue $ T.unwords $ "colvis" : map key fs)
                    H.!? (fieldDisp g, HA.checked "checked")
                    H.! HA.onclick "return colvisSet(event)"
                H.text (fieldTitle g)
            H.td $ when (isNothing (fieldSub g)) (H.text (fieldName f))
            H.td $ H.string (show $ fieldType g)
            H.td H.! HA.class_ "units" $ foldMap H.text (fieldUnits g)
            H.td $ foldMap H.text (fieldDescr g)
        forM_ (fold (fieldSub g)) $ \sf ->
            fielddesc (f <> sf) sf (d+1)
      where
      fs = expandField f
      key = ("colvis-" <>) . fieldName

  case acceptable ["application/json", "text/html"] req of
    Just "application/json" ->
      return $ okResponse [] jcat
    _ -> htmlResponse req [] $ do
      jsonEncodingVar "Catalog" jcat
      jsonVar "Query" query
      H.h2 $ H.text $ catalogTitle cat
      mapM_ (H.p . H.preEscapedText) $ catalogDescr cat

      H.h3 $ "Filters"
      H.p $ "Query and explore a subset using the filters, download your selection using the link below, or get the full dataset above."
      H.table H.! HA.id "filt" $ mempty
      H.div H.! HA.id "dhist" $ do
        H.button H.! HA.id "hist-y-tog" H.! HA.onclick "return toggleLog()" $
          "lin/log"
        H.select H.! HA.id "histsel" H.! HA.onchange "return histogramSelect()" $ do
          H.option H.! HA.value mempty H.! HA.selected "selected" $ "Histogram"
          H.optgroup H.! HA.label "Heatmap" $
            forM_ (catalogFields cat) $ \f ->
              when (typeIsFloating (fieldType f)) $
                H.option H.! HA.value (H.textValue $ fieldName f) $ H.text $ fieldTitle f
        H.div H.! HA.id "hist" $ mempty

      H.h3 $ "Data Table"
      H.table H.! HA.id "tcat" H.! HA.class_ "compact" $ do
        H.thead $ row (fieldsDepth fields) ((id ,) <$> V.toList fields)

      H.h3 $ "Fields Dictionary"
      H.div $ do
        H.button H.! HA.class_ "show_button" H.! HA.onclick "return toggleDisplay('tdict')" $ "show/hide"
        "Table of fields, units, and their descriptions (use checkboxes to view/hide fields in the table above)"
      H.div $ H.table H.! HA.id "tdict" $ do
        H.thead $ H.tr $ do
            H.th $ H.text "Field"
            H.th $ H.text "Variable"
            H.th $ H.text "Type"
            H.th $ H.text "Units"
            H.th $ H.text "Description"
        H.tbody $ forM_ (catalogFieldGroups cat) $ \f -> do
            fielddesc f f 0

      H.h3 $ "Python Query"
      H.div $ do
        H.button H.! HA.class_ "show_button" H.! HA.onclick "return toggleDisplay('div-py')" $ "show/hide"
        "Example python code to apply the above filters and retrieve data. To use, download and install "
        H.a H.! HA.href "https://github.com/flatironinstitute/astrosims-reproto/tree/master/py" $ "this module"
        "."
      H.div H.! HA.id "div-py" $ H.pre H.! HA.id "code-py" $ mempty

sqlSchema :: Route Simulation
sqlSchema = getPath (R.parameter R.>* "schema.sql") $ \sim _ -> do
  cat <- askCatalog sim
  let tab = catalogIndex (catalogStore cat)
  return $ okResponse [] $
    foldMap (\f -> foldMap (\e -> "CREATE TYPE " <> tab <> "_" <> fieldName f <> " AS ENUM(" <> mintersperseMap ", " sqls (V.toList e) <> ");\n") (fieldEnum f)) (catalogFields cat)
    <> "CREATE TABLE " <> tab <> " ("
    <> mintersperseMap "," (\f -> "\n  " <> fieldName f <> " " <> maybe (sqlType (fieldType f)) (\_ -> tab <> "_" <> fieldName f) (fieldEnum f)) (catalogFields cat)
    <> foldMap (\k -> ",\n PRIMARY KEY (" <> k <> ")") (catalogKey cat)
    <> "\n);\n"
    <> "COMMENT ON TABLE " <> tab <> " IS " <> sqls (catalogTitle cat <> foldMap (": " <>) (catalogDescr cat)) <> ";\n"
    <> foldMap (\f -> "COMMENT ON COLUMN " <> tab <> "." <> fieldName f <> " IS " <> sqls (fieldTitle f <> foldMap ((" [" <>) . (<> "]")) (fieldUnits f) <> foldMap (": " <>) (fieldDescr f)) <> ";\n") (catalogFields cat)
  where
  sqls s = "$SqL$" <> s <> "$SqL$" -- hacky dangerous

comparePage :: Route ()
comparePage = getPath "compare" $ \() req -> do
  cats <- asks globalCatalogs
  htmlResponse req [] $ do
    jsonVar "Catalogs" $ catalogMap cats
    jsonVar "Dict" $ catalogDict cats
    H.h2 "Compare"
    H.p $ "Select catalogs across the top to compare, and fields down the left to apply filters and compare statistics and distributions from these catalogs."
    H.table H.! HA.id "tcompare" $ do
      H.thead $ H.tr $ do
        H.th "catalog"
        H.td $ H.select H.! HA.name "selcat" H.! HA.onchange "return selectCat(event.target)" $ do
          H.option H.! HA.value mempty H.! HA.selected "selected" $ "Choose catalog..."
          forM_ (catalogsSorted cats) $ \(sim, cat) ->
            H.option H.! HA.value (H.textValue sim) $ H.text $ catalogTitle cat
      H.tbody $ mempty
      H.tfoot $ do
        H.tr H.! HA.id "tr-add" $
          H.td $ H.select H.! HA.id "addf"  H.! HA.onchange "return addField()"  $ mempty
        H.tr H.! HA.id "tr-comp" $
          H.td $ H.select H.! HA.id "compf" H.! HA.onchange "return compField()" $ mempty
    H.button H.! HA.id "hist-tog" H.! HA.disabled "disabled" H.! HA.onclick "return histogramComp()" $ "histogram"
    H.div H.! HA.id "dhist" $ do
      H.div H.! HA.id "hist-y" $
        H.button H.! HA.id "hist-y-tog" H.! HA.onclick "return toggleLog()" $
          "lin/log"
      H.div H.! HA.id "hist" $ mempty

staticHtml :: Route [FilePathComponent]
staticHtml = getPath ("html" R.*< R.manyI R.parameter) $ \paths q -> do
  let path = FP.joinPath ("html" : map componentFilePath paths) FP.<.> "html"
  fmod <- maybe (result $ response notFound404 [] (path ++ " not found")) return =<<
    liftIO (getModificationTime' path)
  if any (fmod <=) $ parseHTTPDate =<< lookup hIfModifiedSince (Wai.requestHeaders q)
    then return $ Wai.responseBuilder notModified304 [] mempty
    else do
      glob <- ask
      bod <- liftIO $ BSL.readFile path
      htmlResponse q
        [ (hLastModified, formatHTTPDate fmod)
        , (hContentType, "text/html")
        , cacheControl glob q
        ] (H.unsafeLazyByteString bod)

