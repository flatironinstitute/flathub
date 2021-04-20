{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Html
  ( topPage
  , catalogPage
  , sqlSchema
  , csvSchema
  , groupPage
  , comparePage
  , staticHtml
  ) where

import           Control.Monad (forM_, when, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (toUpper)
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict ((!))
import           Data.List (find, inits, sortOn)
import           Data.Maybe (isNothing, isJust)
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.HTTP.Types.Header (ResponseHeaders, hAccept, hIfModifiedSince, hLastModified, hContentType)
import           Network.HTTP.Types.Status (ok200, notModified304, notFound404)
import qualified Network.Wai as Wai
import           Network.Wai.Parse (parseHttpAccept)
import qualified System.FilePath as FP
import qualified Text.Blaze.Html5 as H hiding (text, textValue)
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Internal as H (customParent)
import qualified Text.Hamlet as Hamlet
import qualified Waimwork.Blaze as H (text, textValue, preEscapedBuilder)
import qualified Waimwork.Blaze as WH
import           Waimwork.HTTP (parseHTTPDate, formatHTTPDate)
import           Waimwork.Response (response, okResponse)
import           Waimwork.Result (result)
import qualified Web.Route.Invertible as R
import           Web.Route.Invertible ((!:?))
import qualified Web.Route.Invertible.Render as R

import Type
import Field
import Catalog
import Global
import Compression
import Query
import Monoid
import Attach
import Static

jsonEncodingVar :: T.Text -> J.Encoding -> H.Html
jsonEncodingVar var enc = do
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

vueAttribute :: String -> H.AttributeValue -> H.Attribute
vueAttribute = H.customAttribute . fromString . ("v-" ++)

hamlet :: Hamlet.HtmlUrl R.BoundRoute -> H.Html
hamlet f = f R.renderHamletUrl

unit :: ()
unit = ()

htmlResponse :: Wai.Request -> ResponseHeaders -> H.Markup -> M Wai.Response
htmlResponse _req hdrs body = do
  glob <- ask
  let cats = globalCatalogs glob
  return $ okResponse hdrs $ H.docTypeHtml $ hamlet [Hamlet.hamlet|
    <head>
      $forall src <- [["style.css"],["datatables.min.css"]]
        <link rel="stylesheet" type="text/css" href="@{static !:? src}">
      <!-- TODO: Move mathjax and fonts to bundle.js -->
      <script type=text/javascript src="//cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_CHTML">
      <script>
        #{jsonVar "Catalogs" $ HM.map catalogTitle $ catalogMap cats}
    <body>
      <div .modal-container .hidden #progress-modal>
        <div .modal-background>
          <span>
        <div .modal-body>
          <div .modal-content>
            <h3>Processing...
            <div .progress-container>
              <div .progress-exterior>
                <div .progress-interior>
            <p>One moment, please. The data you requested is being retrieved.
      <header .header>
        <div .header__logo>
          <a href="@{topPage !:? unit}" .header__logo-link>
            <img .header__logo-image src="/web/FlatHubLogo.svg">
        <nav .header__nav>
          <ul #topbar>
            <li .header__link--dropdown>
              <a href="@{groupPage !:? []}">Collections
              <div .dropdown-content>
                $forall g <- groupList $ catalogGroupings cats
                  <a href="@{groupPage !:? [groupingName g]}">
                    <text>#{groupingTitle g $ groupingCatalog cats g}
            <li .header__link--dropdown>
              <a href="@{topPage !:? unit}">Catalogs
              <div .dropdown-content .dropdown-second>
                $forall (key, cat) <- catalogsSorted cats
                  <a href="@{catalogPage !:? key}">
                    <text>#{catalogTitle cat}
            <li .header__link>
              <a href="@{staticHtml !:? ["about"]}">About
      <!--
      <div .subheader>
        <div .subheader-content>
          <p>Please note that this is a beta version. The website is still undergoing final testing before the official release.
      -->
      <div .modal-container .hidden #browser-modal>
        <div .modal-background>
          <span>
        <div .modal-body>
          <div .modal-content>
            <p .modal-close #browser-modal-close onclick="closeModal()">
            <h3>Unsupported Browser
            <p>Flathub requires an up-to-date web browser to make sure that you can use all of the features. Please consider using one of the preferred browsers: Google Chrome, Mozilla Firefox, Apple Safari.
      <div .container .container--main>
        #{body}
      <footer .footer-distributed>
        <div .container>
          <div .footer-center>
            <div .footer__title>
              <img .footer-logo src="/web/FlatHubLogo.svg">
            <p .footer-links>
              <a href="@{topPage !:? unit}">
                <text>Home
              <a href="@{groupPage !:? []}">
                <text>Catalogs
              <!-- <a href="@{comparePage !:? []}">
                <text>Compare -->
              <a href="https://github.com/flatironinstitute/flathub">
                <text>Github
      $forall src <- [["bundle.js"]]
        <script type="text/javascript" src="@{static !:? src}">
    |]

acceptable :: [BS.ByteString] -> Wai.Request -> Maybe BS.ByteString
acceptable l = find (`elem` l) . foldMap parseHttpAccept . lookup hAccept . Wai.requestHeaders

-- Landing page
topPage :: Route ()
topPage = getPath R.unit $ \() req -> do
  cats <- asks globalCatalogs
  case acceptable ["application/json", "text/html"] req of
    Just "application/json" ->
      return $ okResponse [] $ J.encode $ HM.map catalogTitle $ catalogMap cats
    _ -> htmlResponse req [] $ hamlet [Hamlet.hamlet|
      <div>
        <div .section .gray-heading>
          <div .container>
            <div .row>
              <div .heading-content>
                <h4 .heading-heading>Flatiron Institute Data Exploration and Comparison Hub</h4>
        <div .section>
          <div .container>
            <div .row>
              <div .col-md>
                <div .body-copy .narrow-body>
                  <p>
                    <b>Flatiron Institute Data Exploration and Comparison Hub (FlatHUB)</b> is a science platform for diverse
                    types of data, that allows users to explore and compare data from different simulations and datasets with
                    one another and with curated observational/experimental collections. Users can browse and filter the data
                    collections, make simple preview plots, and download sub-samples of the data.
            <div .section>
              <div .box-row>
                <div .box>
                  <div .box-content>
                    <div .box-copy>
                      <div .box-head>Catalogs
                    <ul .link-list>
                      $forall (sim, cat) <- catalogsSorted cats
                        <li>
                          <a .underline href="@{catalogPage !:? sim}">#{catalogTitle cat}
                <div .box>
                  <div .box-content>
                    <div .box-copy>
                      <div .box-head>Collections
                    <ul .link-list>
                      $forall g <- groupList (catalogGroupings cats)
                        <li>
                          <a .underline href="@{groupPage !:? [groupingName g]}">#{groupingTitle g (groupingCatalog cats g)}
      |]

catalogPage :: Route Simulation
catalogPage = getPath R.parameter $ \sim req -> do
  cat <- askCatalog sim
  groups <- asks (catalogGroupings . globalCatalogs)
  let
    fields = catalogFieldGroups cat
    -- rendundant with ToJSON Catalog but with name:
    jcat = J.pairs $
         "name" J..= sim
      <> "title" J..= catalogTitle cat
      <> "descr" J..= catalogDescr cat
      <> foldMap ("count" J..=) (catalogCount cat)
      <> "fields" J..= catalogFields cat
      <> mwhen (not $ null $ catalogSort cat)
        ("sort" J..= catalogSort cat)
    query = parseQuery cat req


  case acceptable ["application/json", "text/html"] req of
    Just "application/json" ->
      return $ okResponse [] jcat
    _ -> htmlResponse req [] $ do
      H.script $ do
        jsonEncodingVar "Catalog" jcat
        jsonVar "Query" query
      H.div H.! HA.class_ "catalog-title" $ do
        H.div H.! HA.class_ "container-fluid" $ do
          H.div H.! HA.class_ "row" $ do
              H.h5 $ H.text $ catalogTitle cat

      H.div H.! HA.class_ "catalog-tool-container " $ do
        H.div H.! HA.class_ "container-fluid catalog-tool" $ do
          H.div H.! HA.class_ "row" $ do
            H.div H.! HA.class_ "col col-sm-12 col-md-8 left-column" $ do
              H.div H.! HA.id "plot" $ do
                -- Start Vue
                H.div H.! HA.class_ "row plot-controls-row" $ do
                  H.div H.! HA.class_ "col-sm-12 col-md-4 plot-col" $ do
                    H.div H.! HA.class_ "tooltip-container" $ do
                      H.span H.! HA.class_ "label-help" $ "Plot type:"
                      H.span H.! HA.class_ "tooltiptext" $ do
                        H.span H.! vueAttribute "if" "type=='x'" $ "Histogram bins the data by a single field and displays count of objects per bin."
                        H.span H.! vueAttribute "if" "type=='y'" $ "Heatmap bins the data by two fields and displays count as color."
                        H.span H.! vueAttribute "if" "type=='c'" $ "Conditional distribution bins the data by one field and displays the distribution of a second field within that bin as a boxplot (range and quartiles)."
                        H.span H.! vueAttribute "if" "type=='s'" $ "Shows a scatterplot of a (random subset) of actual data points."
                    H.select H.! vueAttribute "model" "type" H.! vueAttribute "on:change" "go" $ do
                      H.option H.! HA.value "x" H.! HA.selected "selected" $ "histogram"
                      H.option H.! HA.value "y" $ "heatmap"
                      H.option H.! HA.value "c" $ "conditional distribution"
                      H.option H.! HA.value "s" $ "scatterplot"
                  H.div H.! HA.class_ "col-sm-12 col-md-5 plot-col" $ do
                    forM_ ['x', 'y'] $ \x ->
                      H.div H.! HA.class_ "input-group-row" H.!? (x == 'y', vueAttribute "if" "type!=='x'") H.! vueAttribute "on:change" "go" $ do
                        let filt = H.stringValue [x] <> "filter"
                        H.label $ do
                          H.string [toUpper x] <> "-Axis:"
                        H.div H.! HA.class_ "input-group" $ do
                          H.select H.! HA.id ("plot-" <> H.stringValue [x]) $ do
                            H.option H.! HA.value mempty $ "Choose " <> H.string [toUpper x] <> "-Axis..."
                            forM_ (catalogFields cat) $ \f ->
                              when (typeIsFloating (fieldType f)) $ do
                                let n = H.textValue $ fieldName f
                                H.option
                                  H.! HA.class_ ("sel-" <> n)
                                  H.!? (not $ fieldDisp f, HA.style "display:none")
                                  H.! HA.value n
                                  $ H.text $ fieldTitle f
                          H.div H.! HA.class_ "tooltip-container" H.! vueAttribute "if" filt $ do
                            H.span
                              H.! HA.class_ "tooltiptext"
                              H.! vueAttribute "show" ("!(" <> filt <> ".lbv>0)") $ do
                              "For log functionality, set the filter to enable only positive values."
                            H.div H.! HA.class_ "switch-row" $ do
                              H.label "lin"
                              H.label H.! HA.class_ "switch" $ do
                                H.input
                                  H.! HA.type_ "checkbox"
                                  H.! HA.name ("log" <> H.stringValue [x])
                                  H.! vueAttribute "bind:disabled" ("!(" <> filt <> ".lbv>0)")
                                  H.! vueAttribute "model" (filt <> ".plotLog")
                                H.span H.! HA.class_ "slider" H.! vueAttribute "bind:disabled" ("!(" <> filt <> ".lbv>0)") $ mempty
                              H.label "log"
                  H.div H.! HA.class_ "col-sm-12 col-md-3 plot-col" H.! vueAttribute "if" "type!='s'" $ do
                    H.div H.! HA.class_ "tooltip-container" $ do
                      H.span H.! HA.class_ "label-help" $ "Count:"
                      H.span H.! HA.class_ "tooltiptext" $ do
                        "Choose whether the object count per bin is shown in linear or log scale. "
                        H.span H.! vueAttribute "if" "type=='x'" $ "Count is shown on the Y axis."
                        H.span H.! vueAttribute "else" "" $ "Count is shown on the color (Z) axis."
                    H.div H.! HA.class_ "switch-row solo-row" $ do
                      H.label "lin"
                      H.label H.! HA.class_ "switch" $ do
                        H.input
                          H.! HA.type_ "checkbox"
                          H.! HA.name "log"
                          H.! vueAttribute "model" "log"
                          H.! vueAttribute "on:change" "toggle_log"
                        H.span H.! HA.class_ "slider" $ mempty
                      H.label "log"
                -- End Vue
              H.div H.! HA.class_ "alert alert-danger" H.! HA.id "error" $ mempty
              H.div H.! HA.class_ "plot-container" $ do
                H.div H.! HA.id "plot-chart" $ mempty

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
                    H.div H.! HA.class_ "right-column-heading-group" $ do
                      H.h6 H.! HA.class_ "right-column-heading" $ "Active Filters"
                      H.a H.! HA.class_ "button button-secondary" H.! HA.href (WH.routeActionValue catalogPage sim mempty) $ "reset all"
                    H.div
                      H.! HA.id "filt"
                      H.! HA.class_ "alert-parent" $ do
                      H.div
                        H.! vueAttribute "for" "filter in filters"
                        H.! vueAttribute "bind:id" "'filt-'+filter.field.name"
                        H.! HA.class_ "falert filter-row"
                        H.! vueAttribute "bind:class" "\
                          \{'falert-info':filter.field.flag!==undefined\
                          \,'falert-warning':filter.field.flag===undefined\
                          \,'falert-horz':filter.field.flag===undefined\
                          \}"
                        $ do
                        H.div H.! HA.class_ "filter-text-row" $ do
                          H.div H.! HA.class_ "filter-text" $
                            H.customParent "field-title"
                              H.! vueAttribute "bind:field" "filter.field"
                              H.! vueAttribute "bind:rmf" "filter.field.flag?undefined:filter.remove.bind(filter)"
                              $ mempty
                          H.button H.! HA.class_ "filter-reset"
                            H.! vueAttribute "on:click" "filter.reset()"
                            H.! vueAttribute "if" "filter.field.flag===undefined"
                            $ "reset"
                        H.div
                          H.! HA.class_ "filter-inputs"
                          H.! vueAttribute "if" "filter.field.terms"
                          $ do
                            H.customParent "select-terms"
                              H.! vueAttribute "if" "filter.field.terms"
                              H.! vueAttribute "bind:field" "filter.field"
                              H.! vueAttribute "bind:aggs" "filter.aggs"
                              H.! vueAttribute "model" "filter.value"
                              H.! vueAttribute "bind:change" "filter.change.bind(filter)"
                              $ mempty
                        H.div H.! HA.class_ "filter-inputs-group"
                          H.! vueAttribute "else" "" $ do
                          forM_ [False, True] $ \b ->
                            H.div H.! HA.class_ "filter-input" $ do
                              H.input
                                H.! vueAttribute "bind:name" ("filter.field.name+'." <> (if b then "u" else "l") <> "b'")
                                H.! vueAttribute "bind:title" ("'" <> (if b then "Upper" else "Lower") <> " bound for '+filter.field.title+' values'")
                                H.! HA.type_ "number"
                                H.! vueAttribute "bind:step" "filter.field.base=='i'?'1':'any'"
                                H.! vueAttribute "bind:min" "filter.aggs.min"
                                H.! vueAttribute "bind:max" "filter.aggs.max"
                                H.! vueAttribute "model.number" ("filter." <> (if b then "ubv" else "lbv"))
                                H.! vueAttribute "on:change" "filter.change()"
                        H.div H.! HA.class_ "filter-info-row" H.! vueAttribute "if" "!filter.field.terms" $ do
                          H.div H.! HA.class_ "filter-avg"
                            H.! vueAttribute "if" "filter.aggs.avg!==undefined"
                            $ do
                            H.em $ H.preEscapedString "&mu;"
                            " : {{filter.aggs.avg?filter.render(filter.aggs.avg):'no data'}}"
                          H.div H.! HA.class_ "filter-min-max" $ do
                            H.p $ "Range: "
                            forM_ [False, True] $ \b ->
                              H.p $ " {{filter.render(filter.aggs." <> (if b then "max" else "min") <> ")}}"

                      H.div H.! HA.class_ "alert fade show row filter-row alert-secondary" $ do
                        H.div H.! HA.class_ "filter-text" $
                          "Select field to filter"
                        H.div H.! HA.class_ "filter-inputs" $
                          H.select H.! HA.id "addfilt" H.! HA.onchange "addFilter(event.target.value)" $ do
                            H.option H.! HA.value "" $ "Add filter..."
                            forM_ (catalogFields cat) $ \f -> do
                              let n = H.textValue $ fieldName f
                              H.option
                                H.! HA.value n
                                H.! HA.id ("addfilt-" <> n)
                                H.! HA.class_ ("sel-" <> n)
                                H.!? (not $ fieldDisp f, HA.style "display:none")
                                -- H.! vueAttribute "bind:style" ("{display:disp[" <> WH.lazyByteStringValue (J.encode $ fieldName f) <> "]?'':'none'}")
                                $ H.text $ fieldTitle f
                  H.div H.! HA.class_ "right-column-group" $ do
                    H.h6 H.! HA.class_ "right-column-heading-leader" $ "Random Sample"
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
                        H.! HA.onchange "sampleChange()"
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
                        H.! HA.onchange "sampleChange()"
                H.div
                  H.! HA.class_ "tab-pane fade"
                  H.! HA.id "Python"
                  H.! HA.role "tabpanel"
                  H.! H.customAttribute "aria-labelledby" "python-tab" $ do
                  H.div H.! HA.class_ "right-column-group" $ do
                    H.h6 H.! HA.class_ "right-column-heading" $ "Python Query"
                    H.p $ do
                      "Example python code to apply the above filters and retrieve data. To use, download and install "
                      H.a H.! HA.href "https://github.com/flatironinstitute/flathub/tree/prod/py" $ "this module"
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
                    let gs = findGroupsCatalog sim groups
                    unless (null gs) $ H.div $ do
                      "More under:"
                      H.ul $ forM_ gs $ \g ->
                        H.li $ H.a H.! HA.href (WH.routeActionValue groupPage g mempty) $ mintersperseMap "/" H.text g

        H.div H.! HA.class_ "container-fluid catalog-summary" $ do
          H.div H.! HA.class_ "d-flex justify-content-between" $ do
            H.div H.! HA.class_ "d-flex" $ do
              H.button
                H.! HA.type_ "button"
                H.! HA.class_ "btn btn-warning"
                H.! HA.id "rawdata-btn"
                H.! HA.onclick "toggleShowData()"
                $ "Hide Raw Data"
              H.p H.! HA.class_ "horizontal-label" H.! HA.id "info" $ mempty
            H.div
              H.! HA.id "filt-tab"
              H.! HA.class_ "click-tab-container" $ do
                H.p H.! HA.class_ "horizontal-label" $ "Active Filters"
                H.div
                  H.! HA.class_ "click-tab"
                  H.! vueAttribute "for" "filter in filters"
                  H.! vueAttribute "bind:id" "'filt-tab-'+filter.field.name" $ do
                    -- TODO: Add back in ?
                      -- H.div H.! HA.class_ "click-tab-close" $ mempty
                      H.p
                        H.! vueAttribute "bind:field" "filter.field"
                        H.! HA.class_ "click-tab-content"
                        H.! vueAttribute "bind:value" "filter.field.value" $ H.text "{{filter.field.name}}"
            -- TODO: Fix this download function
            H.div H.! HA.class_ "download-container" H.! HA.id "download" $ do
              H.p H.! HA.class_ "horizontal-label" $ "Format"
              H.select H.! vueAttribute "model" "bulk" $ do
                H.option H.! HA.value mempty $ "Choose format..."
                H.optgroup H.! HA.label "raw data" $ do
                  forM_ [BulkCSV Nothing, BulkCSV (Just CompressionGZip), BulkECSV Nothing, BulkECSV (Just CompressionGZip), BulkNumpy Nothing, BulkNumpy (Just CompressionGZip)] $ \b -> do
                    let f = R.renderParameter b
                    H.option
                      H.! HA.id ("download." <> H.textValue f)
                      H.! HA.class_ "download-option"
                      H.! HA.value (WH.routeActionValue catalogBulk (sim, b) mempty)
                      $ H.text f
                H.optgroup H.! HA.label "attachment files" $ do
                  let att = HM.filter (isJust . fieldAttachment) $ catalogFieldMap cat
                  forM_ att $ \f ->
                    H.option
                      H.! HA.id ("download.attachment." <> H.textValue (fieldName f))
                      H.! HA.class_ "download-option"
                      H.! HA.value (WH.routeActionValue attachmentBulk (sim, fieldName f) mempty)
                      $ H.text (fieldTitle f) <> ".zip"
                  when (not $ HM.null att) $
                    H.option
                      H.! HA.id "download.attachments"
                      H.! HA.class_ "download-option"
                      H.! HA.value (WH.routeActionValue attachmentsBulk sim mempty)
                      $ "all.zip"
              H.a
                H.! HA.class_ "button button-secondary"
                H.! HA.id "download-btn"
                H.! vueAttribute "bind:href" "link"
                $ "Download"

        H.div H.! HA.class_ "container-fluid catalog-summary raw-data" H.! HA.id "rawdata" $ do
          H.div H.! HA.class_ "raw-data__header" $ do
            H.h5 $ "Raw Data"
          H.table H.! HA.id "tcat" $
            H.thead $ do
              row (fieldsDepth fields) ((id ,) <$> V.toList fields)
  where
  fielddesc :: FieldGroup -> FieldGroup -> Int -> H.Html
  fielddesc f g d = do
      H.tr $ do
          H.td H.! HA.class_ ("depth-" <> H.stringValue (show d)) $ do
              H.input H.! HA.type_ "checkbox"
                  H.!? (isNothing (fieldSub g), HA.id $ H.textValue $ key f)
                  H.! HA.class_ (H.textValue $ T.unwords $ "colvis" : map key fs)
                  H.!? (fieldDisp g, HA.checked "checked")
                  H.! HA.onclick "colvisSet(event)"
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
  field d f' f@Field{ fieldSub = Nothing } = H.th
    H.! HA.rowspan (H.toValue d)
    H.! H.dataAttribute "data" (H.textValue $ fieldName f')
    H.! H.dataAttribute "name" (H.textValue $ fieldName f')
    H.! H.dataAttribute "type" (baseType ("num","num","string","string","string") $ fieldType f)
    H.! H.dataAttribute "class-name" (if typeIsNumeric (fieldType f) then "dt-body-right" else "dt-body-left")
    H.!? (not (fieldDisp f), H.dataAttribute "visible" "false")
    H.! H.dataAttribute "default-content" mempty
    H.! HA.class_ "tooltip-dt" $
    fieldBody d f
  field _ _ f@Field{ fieldSub = Just s } = H.th
    H.! HA.colspan (H.toValue $ length $ expandFields s)
    H.! HA.class_ "tooltip-dt" $
    fieldBody 1 f
  row :: Word -> [(FieldGroup -> FieldGroup, FieldGroup)] -> H.Html
  row d l = do
    H.tr $ mapM_ (\(p, f) -> field d (p f) f) l
    when (d > 1) $ row (pred d) $ foldMap (\(p, f) -> foldMap (fmap (p . mappend f, ) . V.toList) $ fieldSub f) l


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

csvSchema :: Route Simulation
csvSchema = getPath (R.parameter R.>* "schema.csv") $ \sim _ -> do
  fields <- if sim == "dict" then asks $ populateDict . globalCatalogs else catalogFields <$> askCatalog sim
  return $ Wai.responseBuilder ok200
    [ (hContentType, "text/csv")
    ]
    $ fieldsCSV fields
  where
  populateDict cats = map pf $ catalogDict cats where
    pf f = f{ fieldDict = Just $ T.intercalate ";" $ md (fieldName f) }
    md d =
      [ c <> "." <> fieldName f <> foldMap (T.cons '[' . (`T.snoc` ']')) (fieldUnits f) <> foldMap (T.cons '*' . T.pack . show) (fieldScale f)
      | (c, cf) <- HM.toList (catalogMap cats)
      , f <- catalogFields cf
      , Just d == fieldDict f
      ]

groupPage :: Route [T.Text]
groupPage = getPath ("group" R.*< R.manyI R.parameter) $ \path req -> do
  cats <- asks globalCatalogs
  grp <- maybe (result $ response notFound404 [] (T.intercalate "/" path <> " not found")) return $
    lookupGrouping path $ catalogGrouping cats
  htmlResponse req [] $ hamlet [Hamlet.hamlet|
    <nav .breadcrumb-container>
      <ol .breadcrumb>
        $forall (p, n) <- zip (inits path) ("collections" : path)
          <li .breadcrumb-item>
            <a href="@{groupPage !:? p}">
              <text>#{n}
    $case grp
      $of GroupCatalog cat
        <!-- Single Catalog -->
        $with Catalog{..} <- catalogMap cats ! cat
          <div .section .gray-heading .#{cat}-heading>
            <div .container>
              <div .row>
                <div .heading-content>
                  <h4 .heading-heading .#{cat}-subheading>
                    <text>#{catalogTitle}
                  <a .button .button-primary href="@{catalogPage !:? cat}">explore
          $maybe html <- catalogHtml
            #{H.preEscapedText html}
          $nothing
            <span .holder>
              <div .section>
                <div .container>
                  <div .row>
                    <div .col-md>
                      <div .body-copy>
                        <h4>Catalog summary
                        $forall desc <- catalogDescr
                          <p>#{H.preEscapedText desc}

      $of Grouping{..}
        <!-- Collections -->
        <div .section .gray-heading .#{groupName}-heading>
          <div .container>
            <div .row>
              <div .heading-content>
                <h4 .heading-heading .#{groupName}-subheading>
                  <text>#{groupTitle}
        <div .section .highlighted-links>
          <div .container>
            <div .row>
              <div .col-md>
                <div .body-copy .group-html>
                  $forall html <- groupHtml
                    #{H.preEscapedText html}
        <div .section>
          <div .container>
            <div .row>
              <div .col-md>
                <div .body-copy>
                  <h4>Catalogs
            <!-- Catalogs list -->
            <div .box-row>
              $forall g <- groupList groupings
                $with cat' <- groupingCatalog cats g
                  <div .box>
                    <div .box-content>
                      <div .box-copy>
                        <div .box-head>
                          <text>#{groupingTitle g cat'}
                        $case g
                          $of Grouping{ groupings = gs }
                            $forall gc <- groupList gs
                              <div .box-desc>
                                <a href="@{groupPage !:? (path ++ [groupingName g, groupingName gc])}">
                                <text>#{groupingTitle gc (groupingCatalog cats gc)}
                          $of GroupCatalog{}
                            $forall cat <- cat'
                              <div .box-desc>
                                $forall syn <- catalogSynopsis cat
                                  #{H.preEscapedText syn}
                      <a .button .button-primary href="@{groupPage !:? (path ++ [groupingName g])}">Learn More
    |]

comparePage :: Route [T.Text]
comparePage = getPath ("compare" R.*< R.manyI R.parameter) $ \path req -> do
  cats <- maybe (result $ response notFound404 [] (T.intercalate "/" path <> " not found")) return .
    groupedCatalogs path =<< asks globalCatalogs
  htmlResponse req [] $ hamlet [Hamlet.hamlet|
    <script>
      #{jsonVar "Catalogs" $ catalogMap cats}
      #{jsonVar "Dict" $ catalogDict cats}
    <div .compare-container>
      <h2>Compare
      <p>Select catalogs across the top to compare, and fields down the left to apply filters and compare statistics and distributions from these catalogs.
      <table #tcompare .u-full-width>
        <thead>
          <tr>
            <th>Choose two or more catalogs
            <td>
              <select name="selcat" onchange="selectCat(event.target)">
                <option value="" selected="selected">Choose catalog...
                $forall (sim, cat) <- catalogsSorted cats
                  <option value="#{sim}">
                    <text>#{catalogTitle cat}
        <tbody>
        <tfoot>
          <tr #tr-add>
            <td>
              <select #addf onchange="addField()">
          <tr #tr-comp>
            <td>
              <select #compf onchange="compField()">
      <button #hist-tog disabled="disabled" onclick="histogramComp()">histogram
      <div #dhist .container>
        <button #hist-y-tog onclick="toggleLog()">
          Toggle lin/log
        <div .container #hist>
    |]

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

