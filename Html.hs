{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Html
  ( topPage
  , catalogPage
  , sqlSchema
  , groupPage
  , comparePage
  , staticHtml
  ) where

import           Control.Monad (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (toUpper)
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import           Data.List (find, inits, sortOn)
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
catalogsSorted = sortOn (catalogOrder . snd) . HM.toList . catalogMap

groupingTitle :: Grouping -> Maybe Catalog -> T.Text
groupingTitle g = maybe (groupTitle g) catalogTitle

htmlResponse :: Wai.Request -> ResponseHeaders -> H.Markup -> M Wai.Response
htmlResponse req hdrs body = do
  glob <- ask
  let isdev = globalDevMode glob && any ((==) "dev" . fst) (Wai.queryString req)
      cats = globalCatalogs glob
  return $ okResponse hdrs $ H.docTypeHtml $ do
    H.head $ do
      forM_ ([["jspm_packages", if isdev then "system.src.js" else "system.js"], ["jspm.config.js"]] ++ if isdev then [["dev.js"]] else [["index.js"]]) $ \src ->
        H.script H.! HA.type_ "text/javascript" H.! HA.src (staticURI src) $ mempty
      -- TODO: use System.resolve:
      forM_ [
          ["jspm_packages", "npm", "datatables.net-dt@1.10.19", "css", "jquery.dataTables.css"],
          ["jspm_packages", "npm", "bootstrap@4.3.1", "dist", "css", "bootstrap.min.css"],
          ["base.css"]
        ] $ \src ->
        H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href (staticURI src)
      H.script H.! HA.type_ "text/javascript" H.! HA.src "//cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_CHTML" $ mempty
      H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href "https://fonts.googleapis.com/css?family=Major+Mono+Display|Montserrat"
      jsonVar "Catalogs" (HM.map catalogTitle $ catalogMap cats)
    H.body $ do
      H.div H.! HA.class_ "modal-container hidden"  H.! HA.id "progress-modal" $ do
        H.div H.! HA.class_ "modal-background" $ do
          H.span $ mempty
        H.div H.! HA.class_ "modal-body" $ do
          H.div H.! HA.class_ "modal-content" $ do
            H.h3 "Processing..."
            H.div H.! HA.class_ "progress-container" $ do
              H.div H.! HA.class_ "progress-exterior" $ do
                H.div H.! HA.class_ "progress-interior" $ mempty
            H.p "One moment, please. The data you requested is being retrieved."
      H.header H.! HA.class_ "header" $ do
        H.div H.! HA.class_ "header__logo" $ do
          H.a H.! HA.href (WH.routeActionValue topPage () mempty) H.! HA.class_ "header__logo-link" $ do
            H.span H.! HA.class_ "header__logo-style" $ "ASTROSIMS"
        H.nav H.! HA.class_ "header__nav" $ do
          H.ul H.! HA.id "topbar" $ do
            H.li H.! HA.class_ "header__link--dropdown" $ do
              H.a H.! HA.href (WH.routeActionValue groupPage [] mempty) $ "Collections"
              H.div H.! HA.class_ "dropdown-content" $ do
                forM_ (groupList $ catalogGroupings cats) $ \g ->
                  H.a H.! HA.href (WH.routeActionValue groupPage [groupingName g] mempty) $ H.text $ groupingTitle g $ groupingCatalog cats g
            H.li H.! HA.class_ "header__link--dropdown" $ do
              H.a H.! HA.href (WH.routeActionValue topPage () mempty) $ "Catalogs"
              H.div H.! HA.class_ "dropdown-content dropdown-second" $ do
                forM_ (catalogsSorted cats) $ \(key, cat) ->
                  H.a H.! HA.href (WH.routeActionValue catalogPage key mempty) $ H.text (catalogTitle cat)
            H.li H.! HA.class_ "header__link" $ do
              H.a H.! HA.href (WH.routeActionValue comparePage [] mempty) $ "Compare"
            H.li H.! HA.class_ "header__link" $ do
              H.a H.! HA.href (WH.routeActionValue staticHtml ["about"] mempty) $ "About"
      H.div H.! HA.class_ "container container--main" $ do
        body
      H.footer H.! HA.class_"footer-distributed" $ do
        H.div H.! HA.class_ "container" $ do
          H.div H.! HA.class_ "footer-left" $ do
            H.div H.! HA.class_ "footer__title" $ do
              H.img H.! HA.src (staticURI ["cca-logo.jpg"]) H.! HA.height "50" H.! HA.width "50"
              H.h3 $ "Center for Computational Astrophysics"
            H.p H.! HA.class_ "footer-links" $ do
              H.a H.! HA.href (WH.routeActionValue topPage () mempty) $ H.text "Home"
              H.a H.! HA.href (WH.routeActionValue groupPage [] mempty) $ H.text "Catalogs"
              H.a H.! HA.href (WH.routeActionValue comparePage [] mempty) $ H.text "Compare"
              H.a H.! HA.href "https://github.com/flatironinstitute/astrosims" $ H.text "Github"
            H.p H.! HA.class_ "footer-company-name" $ "Flatiron Institute, 2019"

acceptable :: [BS.ByteString] -> Wai.Request -> Maybe BS.ByteString
acceptable l = find (`elem` l) . foldMap parseHttpAccept . lookup hAccept . Wai.requestHeaders

-- Landing page
topPage :: Route ()
topPage = getPath R.unit $ \() req -> do
  cats <- asks globalCatalogs
  case acceptable ["application/json", "text/html"] req of
    Just "application/json" ->
      return $ okResponse [] $ J.encode $ HM.map catalogTitle $ catalogMap cats
    _ -> htmlResponse req [] $
      H.div $ do
        H.div H.! HA.class_ "section hero gray-heading" $ do
          H.div H.! HA.class_ "container" $ do
            H.div H.! HA.class_ "row" $ do
              H.div H.! HA.class_ "hero-content" $ do
                H.h4 H.! HA.class_ "hero-heading" $ "ASTROSIMS"
                H.h4 H.! HA.class_ "hero-subheading" $ mempty
        H.div H.! HA.class_ "section" $ do
          H.div H.! HA.class_ "container" $ do
            H.div H.! HA.class_ "row" $ do
              H.div H.! HA.class_ "col-md" $ do
                H.div H.! HA.class_ "collections" $ do
                  H.div H.! HA.class_ "collections-container" $ do
                    H.h3 H.! HA.class_ "section__heading" $ H.a H.! HA.href (WH.routeActionValue groupPage [] mempty) $ "Collections"
                    H.p H.! HA.class_ "section-description" $ mempty
                    H.div H.! HA.class_ "row" $ do
                      H.div H.! HA.class_ "collections-list" $ do
                        forM_ (groupList $ catalogGroupings cats) $ \g ->
                            H.a H.! HA.href (WH.routeActionValue groupPage [groupingName g] mempty) H.! HA.class_ "collection-card-heading" $ H.text $ groupingTitle g $ groupingCatalog cats g
              H.div H.! HA.class_ "col-md col-border" $ do
                H.div H.! HA.class_ "catalogs" $ do
                  H.div H.! HA.class_ "catalogs-container" $ do
                    H.h3 H.! HA.class_ "section__heading" $ "Catalogs"
                    H.p H.! HA.class_ "section-description" $ "Simulation datasets"
                    H.div H.! HA.class_ "catalogs-list main-page" $
                      forM_ (catalogsSorted cats) $ \(sim, cat) -> do
                          H.a H.! HA.href (WH.routeActionValue catalogPage sim mempty) H.! HA.class_ "collection-card-heading" $ H.text $ catalogTitle cat


catalogPage :: Route Simulation
catalogPage = getPath R.parameter $ \sim req -> do
  cat <- askCatalog sim
  let
    fields = catalogFieldGroups cat
    fields' = catalogFields cat
    jcat = J.pairs $
         "name" J..= sim
      <> "title" J..= catalogTitle cat
      <> "descr" J..= catalogDescr cat
      <> "bulk" J..= map (J.String . R.renderParameter) [BulkCSV Nothing, BulkCSV (Just CompressionGZip), BulkECSV Nothing, BulkECSV (Just CompressionGZip), BulkNumpy Nothing, BulkNumpy (Just CompressionGZip)]
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
          H.! HA.class_ "tooltip-dt" $
        fieldBody d f
    field _ _ f@Field{ fieldSub = Just s } = do
      H.th
          H.! HA.colspan (H.toValue $ length $ expandFields s)
          H.! HA.class_ "tooltip-dt" $
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
      H.div H.! HA.class_ "catalog-title" $ do
        H.div H.! HA.class_ "container" $ do
          H.div H.! HA.class_ "row" $ do
            H.div H.! HA.class_ "col" $ do
              H.h5 $ H.text $ catalogTitle cat

      H.div H.! HA.class_ "catalog-tool-container " $ do
        H.div H.! HA.class_ "container-fluid catalog-tool" $ do
          H.div H.! HA.class_ "row" $ do
            H.div H.! HA.class_ "col col-sm-12 col-md-8 left-column" $ do
              forM_ ['x', 'y'] $ \x ->
                H.div H.! HA.class_ "left-column-header-group" $ do
                  H.div H.! HA.class_ "form-inline form-badge" $ do
                    H.div H.! HA.class_ "form-group" $ do
                      H.label H.! HA.for ("histsel-" <> H.stringValue [x]) $ do
                        H.a H.! HA.class_ "nav-link" $ "Select " <> H.string [toUpper x] <> " Axis"
                      H.select H.! HA.id ("histsel-" <> H.stringValue [x]) $ do
                        forM_ (catalogFields cat) $ \f ->
                          when (typeIsFloating (fieldType f)) $
                            H.option
                              H.! HA.class_ ("sel-" <> H.textValue (fieldName f))
                              H.!? (not $ fieldDisp f, HA.style "display:none")
                              H.! HA.value (H.textValue $ fieldName f) $ H.text $ fieldTitle f
                    H.button H.! HA.class_ "btn btn-badge-inline" H.! HA.onclick ("return histogramShow(" <> H.stringValue (show x) <> ")") $ do
                      "View "
                      if x == 'x' then "Histogram" else "Heatmap"
                    when (x == 'x') $
                      H.button H.! HA.id "hist-y-tog" H.! HA.class_ "btn btn-badge-inline" H.! HA.onclick "return toggleLog()" $ "Toggle lin/log"
                    when (x == 'y') $
                      H.button H.! HA.class_ "btn btn-badge-inline" H.! HA.onclick ("return histogramShow('c')") $
                        "Conditional distribution"
              H.div H.! HA.class_ "alert alert-danger" H.! HA.id "error" $ mempty
              H.div H.! HA.id "hist" $ mempty

            H.div H.! HA.class_ "col col-sm-12 col-md-4 right-column" $ do
              H.ul H.! HA.class_ "nav nav-tabs" H.! HA.id "myTab" H.! HA.role "tablist" $ do
                forM_ ["Filter", "Python", "Fields", "About"] $ \t -> do
                  H.li H.! HA.class_ "nav-item" $ do
                    H.a
                      H.! HA.class_ ("nav-link" <> if t == "Filter" then " active" else mempty)
                      H.! HA.id (H.stringValue t <> "-tab")
                      H.! H.dataAttribute "toggle" "tab"
                      H.! HA.href ("#" <> H.stringValue t)
                      H.! HA.role "tab"
                      H.! H.customAttribute "aria-controls" "filter"
                      H.! H.customAttribute "aria-selected" "true"
                      $ H.string (if t == "Fields" then "All Fields" else t)
              H.div H.! HA.class_ "tab-content" H.! HA.id "myTabContent" $ do
                H.div
                  H.! HA.class_ "tab-pane fade show active right-column-container"
                  H.! HA.id "Filter"
                  H.! HA.role "tabpanel"
                  H.! H.customAttribute "aria-labelledby" "filter-tab" $ do
                  H.div H.! HA.class_ "right-column-group" $ do
                    H.h6 H.! HA.class_ "right-column-heading" $ "Active Filters"
                    H.div
                      H.! HA.id "filt"
                      H.! HA.class_ "alert-parent"
                      H.! HA.role "alert" $ mempty
                  H.div H.! HA.class_ "right-column-group" $ do
                    H.h6 H.! HA.class_ "right-column-heading" $ "Random Sample"
                    H.div H.! HA.class_ "sample-row" $ do
                      H.label H.! HA.for "sample" $ "fraction"
                      H.input
                        H.! HA.id "sample"
                        H.! HA.name "sample"
                        H.! HA.type_ "number"
                        H.! HA.step "any"
                        H.! HA.min "0"
                        H.! HA.max "1"
                        H.! HA.value (H.toValue $ querySample query)
                        H.! HA.title "Probability (0,1] with which to include each item"
                        H.! HA.onchange "return sampleChange()"
                      H.label H.! HA.for "seed" $ "seed"
                      H.input
                        H.! HA.id "seed"
                        H.! HA.name "seed"
                        H.! HA.type_ "number"
                        H.! HA.step "1"
                        H.! HA.min "0"
                        WH.!? (HA.value . H.toValue <$> querySeed query)
                        H.!? (querySample query < 1, HA.disabled "disabled")
                        H.! HA.title "Random seed to generate sample selection"
                        H.! HA.onchange "return sampleChange()"
                H.div
                  H.! HA.class_ "tab-pane fade"
                  H.! HA.id "Python"
                  H.! HA.role "tabpanel"
                  H.! H.customAttribute "aria-labelledby" "python-tab" $ do
                  H.div H.! HA.class_ "right-column-group" $ do
                    H.h6 H.! HA.class_ "right-column-heading" $ "Python Query"
                    H.p $ do
                      "Example python code to apply the above filters and retrieve data. To use, download and install "
                      H.a H.! HA.href "https://github.com/flatironinstitute/astrosims-reproto/tree/master/py" $ "this module"
                      "."
                    H.div H.! HA.id "div-py" H.! HA.class_ "python-block" $
                      H.pre H.! HA.id "code-py" $ mempty
                H.div
                  H.! HA.class_ "tab-pane fade"
                  H.! HA.id "Fields"
                  H.! HA.role "tabpanel"
                  H.! H.customAttribute "aria-labelledby" "dict-tab" $ do
                  H.div H.! HA.class_ "right-column-group" $ do
                    H.h6 H.! HA.class_ "right-column-heading" $ "Fields Dictionary"
                    H.div $ H.table H.! HA.id "tdict" H.! HA.class_ "table table-striped table-sm" $ do
                      H.thead $ H.tr $ do
                          H.th $ "Field"
                          H.th $ "Variable"
                          H.th $ "Type"
                          H.th $ "Units"
                          H.th $ "Description"
                      H.tbody $ forM_ (catalogFieldGroups cat) $ \f -> do
                          fielddesc f f 0
                H.div
                  H.! HA.class_ "tab-pane fade"
                  H.! HA.id "About"
                  H.! HA.role "tabpanel"
                  H.! H.customAttribute "aria-labelledby" "about-tab" $ do
                  H.div H.! HA.class_ "right-column-group" $ do
                    H.h6 H.! HA.class_ "right-column-heading" $ "About Catalog"
                    mapM_ (H.p . H.preEscapedText) $ catalogDescr cat

        H.div H.! HA.class_ "container-fluid catalog-summary" $ do
          -- H.p H.! HA.id "count" $ mempty
          H.div H.! HA.class_ "d-flex justify-content-between" $ do
            H.div H.! HA.class_ "d-flex" $ do
              H.button
                H.! HA.type_ "button"
                H.! HA.class_ "btn btn-warning"
                H.! HA.id "rawdata-btn"
                H.! HA.onclick "return toggleShowData()"
                $ "Hide Raw Data"
              H.p H.! HA.class_ "download" H.! HA.id  "info" $ mempty
            H.p H.! HA.class_ "download" H.! HA.id "download" $ "Download as:"

        H.div H.! HA.class_ "container-fluid catalog-summary raw-data" H.! HA.id "rawdata" $ do
          H.h5 $ "Raw Data"
          H.table H.! HA.id "tcat" $ do
            H.thead $ row (fieldsDepth fields) ((id ,) <$> V.toList fields)


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

groupPage :: Route [T.Text]
groupPage = getPath ("group" R.*< R.manyI R.parameter) $ \path req -> do
  cats <- asks globalCatalogs
  grp <- maybe (result $ response notFound404 [] (T.intercalate "/" path <> " not found")) return $
    lookupGrouping path $ catalogGrouping cats
  htmlResponse req [] $ do
    H.nav H.! HA.class_"breadcrumb-container" $ do
      H.ol H.! HA.class_"breadcrumb" $ fold $ zipWith (\p n ->
        H.li H.! HA.class_ "breadcrumb-item" $ do
          H.a H.! HA.href (WH.routeActionValue groupPage p mempty) $ H.text n)
        (inits path) ("collections":path)
    case grp of
      -- Single Catalog
      GroupCatalog cat -> do
        let Catalog{..} = catalogMap cats HM.! cat
        H.div H.! HA.class_ "section gray-heading" $ do
          H.div H.! HA.class_"container" $ do
            H.div H.! HA.class_ "row" $ do
              H.div H.! HA.class_ "heading-content" $ do
                H.h4 H.! HA.class_"heading-heading"  $ H.text catalogTitle
                H.a  H.! HA.class_ "button button-primary" H.! HA.href (WH.routeActionValue catalogPage cat mempty) $ "explore"
        maybe (do
          H.div H.! HA.class_ "section highlighted-links" $ do
            mapM_ (H.p . H.preEscapedText) catalogDescr)
            H.preEscapedText catalogHtml
      -- Collections
      Grouping{..} -> do
        H.div H.! HA.class_ "section gray-heading" $ do
          H.div H.! HA.class_"container" $ do
            H.div H.! HA.class_ "row" $ do
              H.div H.! HA.class_ "heading-content" $ do
                H.h4 H.! HA.class_"heading-heading"  $ H.text groupTitle
                H.a H.! HA.class_ "button button-primary" H.! HA.href (WH.routeActionValue comparePage path mempty) $ "compare"
        mapM_
          H.preEscapedText groupHtml
        H.dl H.! HA.class_ "section catalogs-list" $
          forM_ (groupList groupings) $ \g -> do
            let cat' = groupingCatalog cats g
            H.dt $ H.a H.! HA.href (WH.routeActionValue groupPage (path ++ [groupingName g]) mempty) $
              H.text $ groupingTitle g cat'
            case g of
              Grouping{ groupings = gs } ->
                forM_ (groupList gs) $ \gc ->
                  H.dd $ H.a H.! HA.href (WH.routeActionValue groupPage (path ++ [groupingName g, groupingName gc]) mempty) $
                    H.text $ groupingTitle gc (groupingCatalog cats gc)
              GroupCatalog{} ->
                forM_ cat' $ \cat -> H.dd $ do
                  mapM_ H.preEscapedText $ catalogDescr cat
                  mapM_ ((" " <>) . (<> " rows.") . H.toMarkup) $ catalogCount cat

comparePage :: Route [T.Text]
comparePage = getPath ("compare" R.*< R.manyI R.parameter) $ \path req -> do
  cats <- maybe (result $ response notFound404 [] (T.intercalate "/" path <> " not found")) return .
    groupedCatalogs path =<< asks globalCatalogs
  htmlResponse req [] $ do
    jsonVar "Catalogs" $ catalogMap cats
    jsonVar "Dict" $ catalogDict cats
    H.div H.! HA.class_ "container container--main" $ do
      H.h2 "Compare"
      H.p $ "Select catalogs across the top to compare, and fields down the left to apply filters and compare statistics and distributions from these catalogs."
      H.table H.! HA.id "tcompare" H.! HA.class_ "u-full-width" $ do
        H.thead $ H.tr $ do
          H.th "Choose two or more catalogs"
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
        H.button H.! HA.id "hist-y-tog" H.! HA.onclick "return toggleLog()" $
          "Toggle lin/log"
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

