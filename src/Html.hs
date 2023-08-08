{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Html
  ( topPage
  , catalogPage
  , groupPage
  , comparePage
  , staticHtml
  , firePage
  , agoraPage
  , sqlSchema
  , csvSchema
  , attachment
  ) where

import           Control.Monad (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict ((!)) -- hamlet doesn't like qualified operators
import           Data.List (find, inits, sortOn, intercalate, nub)
import           Data.Maybe (isNothing, isJust, maybeToList)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.HTTP.Types.Header (Header, ResponseHeaders, hAccept, hIfModifiedSince, hLastModified, hContentType, hLocation)
import qualified Network.HTTP.Types.URI as U
import           Network.HTTP.Types.Status (notModified304, permanentRedirect308)
import qualified Network.Wai as Wai
import           Network.Wai.Parse (parseHttpAccept)
import qualified System.FilePath as FP
import qualified Text.Blaze.Html5 as H hiding (text, textValue)
import qualified Text.Hamlet as Hamlet
import qualified Waimwork.Blaze as H (text, preEscapedBuilder)
import           Waimwork.HTTP (parseHTTPDate, formatHTTPDate)
import           Waimwork.Response (okResponse, response)
import qualified Web.Route.Invertible as R
import           Web.Route.Invertible (BoundRoute((:?)), (!:?))
import qualified Web.Route.Invertible.Internal as R (requestRoute')
import qualified Web.Route.Invertible.Render as R
import qualified Web.Route.Invertible.Wai as R

import Type
import Field
import Catalog
import Error
import Global
import Compression
import Query
import Monoid
import Static
import Api
import qualified KeyedMap as KM

locationHeader :: Wai.Request -> R.Route a -> a -> U.Query -> Header
locationHeader req r a q = (hLocation, BSL.toStrict $ B.toLazyByteString $
  R.renderRequestBuilder (R.requestRoute' r a (R.waiRequest req)) q)

jsonEncodingVar :: T.Text -> J.Encoding -> H.Html
jsonEncodingVar var enc = do
  H.text var
  "="
  H.preEscapedBuilder $ J.fromEncoding enc
  ";"

jsonVar :: J.ToJSON a => T.Text -> a -> H.Html
jsonVar var = jsonEncodingVar var . J.toEncoding

catalogsSorted :: Catalogs -> [Catalog]
catalogsSorted Catalogs{..} = nub $ filter catalogVisible $ gcs catalogGroupings ++ sortOn catalogName (HM.elems catalogMap) where
  gcs = foldMap gc . V.toList . groupList
  gc (GroupCatalog c) = maybeToList $ HM.lookup c catalogMap
  gc Grouping{..} = gcs groupings

groupingTitle :: Grouping -> Maybe Catalog -> T.Text
groupingTitle g = maybe (groupTitle g) catalogTitle

hamlet :: Hamlet.HtmlUrl R.BoundRoute -> H.Html
hamlet f = f R.renderHamletUrl

htmlResponse :: Wai.Request -> ResponseHeaders -> H.Markup -> M Wai.Response
htmlResponse _req hdrs body = do
  glob <- ask
  let cats = globalCatalogs glob
  return $ okResponse hdrs $ H.docTypeHtml $ hamlet [Hamlet.hamlet|
    <head>
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <meta name="description" content="Flatiron Institute Data Exploration and Comparison Hub">
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
      <title>FlatHUB
      $forall src <- [["style.css"],["datatables.min.css"]]
        <link rel="stylesheet" type="text/css" href="@{static !:? src}">
      <!-- TODO: Move mathjax and fonts to bundle.js -->
      <script type=text/javascript src="//cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_CHTML">
      <script>
        #{jsonVar "Catalogs" $ HM.map catalogTitle $ catalogMap cats}
      <script>
        window.onUsersnapCXLoad = function(api) {
          api.init();
        }
      <script type=text/javascript defer=1 src="https://widget.usersnap.com/global/load/a964b199-4df3-4ba5-82d0-44fffad1ac0c?onload=onUsersnapCXLoad">
    <body>
      <div .modal-container #progress v-show="update" style="display: none;">
        <div .modal-background>
          <span>
        <div .modal-body>
          <div .modal-content>
            <h3>Processing...
            <div .progress-container>
              <div .progress-exterior>
                <div .progress-interior>
            <p>One moment, please. The data you requested is being retrieved.
            <button v-on:click="cancel">Cancel
      <header .header>
        <div .header__logo>
          <a href="@{topPage !:? mempty}" .header__logo-link>
            <img .header__logo-image src="/web/FlatHubLogo.svg">
        <nav .header__nav>
          <ul #topbar>
            <li .header__link--dropdown>
              <a href="@{groupPage !:? []}">Collections
              <div .dropdown-content>
                $forall g <- groupList $ catalogGroupings cats
                  $if groupVisible g
                    <a href="@{groupPage !:? [groupingName g]}">
                      <text>#{groupingTitle g $ groupingCatalog cats g}
            <li .header__link--dropdown>
              <a href="@{topPage !:? mempty}">Catalogs
              <div .dropdown-content .dropdown-second>
                <a href="@{agoraPage !:? mempty}"><text>AGORA
                <a href="@{firePage !:? mempty}"><text>FIRE
                $forall cat <- catalogsSorted cats
                  <a href="@{catalogPage !:? catalogName cat}">
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
              <a href="@{topPage !:? mempty}">
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
      return $ okResponse [] $ J.toEncoding $ HM.map catalogTitle $ catalogMap cats
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
                      <li>
                        <a .underline href="@{agoraPage !:? mempty}">AGORA
                      <li>  
                        <a .underline href="@{firePage !:? mempty}">FIRE
                      $forall cat <- catalogsSorted cats
                        <li>
                          <a .underline href="@{catalogPage !:? catalogName cat}">#{catalogTitle cat}
                <div .box>
                  <div .box-content>
                    <div .box-copy>
                      <div .box-head>Collections
                    <ul .link-list>
                      $forall g <- groupList (catalogGroupings cats)
                        $if groupVisible g
                          <li>
                            <a .underline href="@{groupPage !:? [groupingName g]}">#{groupingTitle g (groupingCatalog cats g)}
      |]


data Axis = AxisX | AxisY | AxisZ
  deriving (Eq)

instance H.ToMarkup Axis where
  toMarkup AxisX = H.string "x"
  toMarkup AxisY = H.string "y"
  toMarkup AxisZ = H.string "z"

instance Show Axis where
  show AxisX = "X"
  show AxisY = "Y"
  show AxisZ = "Z"

ifs :: Bool -> String -> String -> String
ifs True a _ = a
ifs False _ a = a

catalogPage :: Route Simulation
catalogPage = getPath R.parameter $ \sim req -> do
  cat <- askCatalog sim
  groups <- asks (catalogGroupings . globalCatalogs)
  let
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
    fielddisps = KM.fromList $ queryFields query
    fielddisp :: Field -> Bool
    fielddisp = (`KM.member` fielddisps)
    fields = catalogFieldGroups cat
    toprow = row (fieldsDepth fields) fields
    fielddesc :: Field -> Int -> H.Html
    fielddesc f d = do
      hamlet [Hamlet.hamlet|
        <tr>
          <td .depth-#{d}>
            <input type="checkbox"
                :isNothing (fieldSub f):id="#{key f}"
                class="colvis #{T.unwords $ map key $ V.toList fs}"
                :fielddisp f:checked=checked
                onclick="colvisSet(event)">
            #{fieldTitle f}
          <td>
            $if isNothing (fieldSub f)
              #{fieldName f}
          <td>#{show $ fieldType f}
          <td .units>
            $forall u <- fieldUnits f
              #{u}
          <td>
            $forall d <- fieldDescr f
              #{d}
        |]
      forM_ (fold (fieldSub f)) $ \sf ->
        fielddesc sf (d+1)
      where
      fs = expandField f
      key = ("colvis-" <>) . fieldName
    fieldBody :: Word -> Field -> H.Html
    fieldBody d f = hamlet [Hamlet.hamlet|
      <span>
        <!-- Writes the title to the span -->
        #{fieldTitle f}
        <!-- Writes the unit to the span -->
        $forall u <- fieldUnits f
          $if d > 1
            <br>
          <span class="units">#{H.preEscapedText u}
        $forall d <- fieldDescr f
          <span class="tooltiptext">#{d}
      |]
    field :: Word -> Field -> H.Html
    field d f@Field{ fieldSub = Nothing } = hamlet [Hamlet.hamlet|
      <th .tooltip-dt
        rowspan=#{d}
        data-data=#{fieldName f}
        data-name=#{fieldName f}
        data-type=#{baseType (asTypeOf "num" T.empty,"num","string","string","string") $ fieldType f}
        data-class-name="dt-body-#{ifs (typeIsNumeric (fieldType f)) "right" "left"}"
        :not (fielddisp f):data-visible="false"
        :or (fieldStore f):data-orderable="false"
        data-default-content="">
        #{fieldBody d f}
      |]
    field _ f@Field{ fieldSub = Just s } = hamlet [Hamlet.hamlet|
      <th .tooltip-dt
        colspan=#{V.length $ expandFields s}>
        #{fieldBody 1 f}
      |]
    row :: Word -> Fields -> H.Html
    row d l = do
      H.tr $ mapM_ (field d) l
      when (d > 1) $ row (pred d) $ foldMap (fold . fieldSub) l

  case acceptable ["application/json", "text/html"] req of
    Just "application/json" ->
      return $ okResponse [] jcat
    _ -> htmlResponse req [] $ hamlet [Hamlet.hamlet|
      <script>
        #{jsonEncodingVar "Catalog" jcat}
        #{jsonVar "Query" query}
      <div .catalog-title>
        <div .container-fluid>
          <div .row>
              <h5>
                <text>#{catalogTitle cat}

      <div .catalog-tool-container>
        <div .container-fluid .catalog-tool>
          <div .row>
            <div .col .col-sm-12 .col-md-8 .left-column>
              <div #plot>
                <!-- Start Vue -->
                <div .row .plot-controls-row>
                  <div .col-sm-12 .col-md-4 .plot-col>
                    <div .tooltip-container>
                      <span .label-help>Plot type:
                      <span .tooltiptext>
                        <span v-if="type=='x'">Histogram bins the data by a single field and displays count of objects per bin.
                        <span v-else-if="type=='y'">Heatmap bins the data by two fields and displays count as color.
                        <span v-else-if="type=='c'">Conditional distribution bins the data by one field and displays the distribution of a second field within that bin as a boxplot (range and quartiles).
                        <span v-else-if="type=='s'">Shows a scatterplot of a (random subset) of actual data points.
                    <select v-model="type" v-on:change="go">
                      <option value="x" selected=selected>histogram
                      <option value="y">heatmap
                      <option value="c">conditional distribution
                      <option value="s">scatterplot
                  <div .col-sm-12 .col-md-5 .plot-col>
                    $forall axis <- [AxisX,AxisY,AxisZ]
                      <div .input-group-row :axis == AxisY:v-if="type!=='x'" :axis == AxisZ:v-if="type==='s'" v-on:change="go">
                        <label>
                          #{show axis}-Axis:
                        <div .input-group>
                          <select #plot-#{axis}>
                            <option value="">Choose #{show axis}-Axis...
                            $forall f <- catalogFields cat
                              $if typeIsFloating (fieldType f) && not (or (fieldStore f))
                                <option
                                  .sel-#{fieldName f}
                                  :not (fielddisp f):style="display:none"
                                  value="#{fieldName f}">
                                  #{fieldTitle f}
                          <div .tooltip-container v-if="filter.#{axis}">
                            <span .tooltiptext v-show="!(filter.#{axis}.lbv>0)">
                              For log functionality, set the filter to enable only positive values.
                            <div .switch-row>
                              <label>lin
                              <label .switch>
                                <input
                                  type="checkbox"
                                  name="log#{axis}"
                                  v-bind:disabled="!(filter.#{axis}.lbv>0)"
                                  v-model="filter.#{axis}.plotLog">
                                <span .slider v-bind:disabled="!(filter.#{axis}.lbv>0)">
                              <label>log
                    <div .input-group-row v-if="type==='s'" v-on:change="go">
                      <label>
                        Color:
                      <div .input-group>
                        <select #plot-c>
                          <option value="">None
                          $forall f <- catalogFields cat
                            $if (fieldTerms f || not (typeIsString (fieldType f))) && not (or (fieldStore f))
                              <option
                                .sel-#{fieldName f}
                                :not (fielddisp f):style="display:none"
                                value="#{fieldName f}">
                                #{fieldTitle f}
                        <div .tooltip-container>
                          <div .switch-row>
                            <label>lin
                            <label .switch>
                              <input
                                type="checkbox"
                                name="logcolor"
                                v-bind:disabled="!color || color.terms"
                                v-model="colorlog">
                              <span .slider v-bind:disabled="!color || color.terms">
                            <label>log
                  <div .col-sm-12 .col-md-3 .plot-col v-if="type!='s'">
                    <div .tooltip-container>
                      <span .label-help>Count:
                      <span .tooltiptext>
                        Choose whether the object count per bin is shown in linear or log scale.
                        <span v-if="type=='x'">Count is shown on the Y axis.
                        <span v-else>Count is shown on the color (Z) axis.
                    <div .switch-row .solo-row>
                      <label>lin
                      <label .switch>
                        <input
                          type="checkbox"
                          name="log"
                          v-model="log"
                          v-on:change="toggle_log">
                        <span .slider>
                      <label>log
                <!-- End Vue -->
              <div .alert .alert-danger #error>
              <div .plot-container>
                <div #plot-chart>

            <div .col .col-sm-12 .col-md-4 .right-column>
              <ul .nav .nav-tabs #myTab role="tablist">
                $forall t <- [asTypeOf "Filter" T.empty, "Python", "Fields", "About"]
                  <li .nav-item>
                    <a ##{t}-tab .nav-link
                      :t == "Filter":class="active"
                      data-toggle=tab
                      href="##{t}"
                      role="tab"
                      aria-controls="filter"
                      aria-selected="true">
                      <string>
                        $if t == "Fields"
                          All Fields
                        $else
                          #{t}
              <div .tab-content #myTabContent>
                <div .tab-pane .fade .show .active .right-column-container #Filter role="tabpanel" aria-labelledby="filter-tab">
                  <div .right-column-group>
                    <div .right-column-heading-group>
                      <h6 .right-column-heading>Active Filters
                      <a .button .button-secondary href="@{catalogPage !:? sim}">reset all
                    <div #filt .alert-parent>
                      <div
                        v-for="filter in filters"
                        v-bind:id="'filt-'+filter.field.name"
                        .falert .filter-row
                        v-bind:class="{'falert-info':filter.field.flag!==undefined,'falert-warning':filter.field.flag===undefined,'falert-horz':filter.field.flag===undefined}">
                        <div .filter-text-row>
                          <field-title
                            v-bind:field="filter.field"
                            v-bind:rmf="filter.field.flag?undefined:filter.remove.bind(filter)">
                        <div .filter-inputs v-if="filter.field.terms">
                          <select-terms
                              v-bind:field="filter.field"
                              v-bind:aggs="filter.aggs"
                              v-model="filter.value"
                              v-bind:change="filter.change.bind(filter)">
                        <div .filter-inputs v-else-if="filter.field.base==='s'">
                          <span v-if="filter.field.wildcard">Use "*" as wildcard</span>
                          <input
                            type="text"
                            v-bind:name="filter.field.name"
                            v-model="filter.value"
                            v-on:change="filter.change()">
                        <div .filter-inputs-group v-else>
                          $forall b <- [False, True]
                            <div .filter-input>
                              <input
                                type="number"
                                v-bind:name="filter.field.name+'.#{ifs b "u" "l"}b'"
                                v-bind:title="'#{ifs b "Upper" "Lower"} bound for '+filter.field.title+' values'"
                                v-bind:step="filter.field.base=='i'?'1':'any'"
                                v-bind:min="filter.aggs.min"
                                v-bind:max="filter.aggs.max"
                                v-model.number="filter.#{ifs b "u" "l"}bv"
                                v-on:change="filter.change()">
                        <div .filter-info-row v-if="!filter.field.terms">
                          <div .filter-avg v-if="filter.aggs.avg!==undefined">
                            <em>&mu; : {{filter.aggs.avg?filter.render(filter.aggs.avg):'no data'}}
                          <div .filter-examples v-if="filter.aggs.buckets!==undefined">
                            Examples:
                              <span class="example-item" v-for="bucket in filter.aggs.buckets">
                                {{ bucket.key }}
                          <div v-else .filter-min-max>
                            <p>Range:
                            $forall b <- [False, True]
                              <p>{{filter.render(filter.aggs.#{ifs b "max" "min"})}}

                      <div .falert .filter-row .alert-secondary>
                        <div .filter-text>Select field to filter
                        <div .filter-inputs>
                          <select #addfilt onchange="addFilter(event.target.value)">
                            <option value="">Add filter...
                            $forall f <- catalogFields cat
                              $if not (or (fieldStore f))
                                <option #addfilt-#{fieldName f}
                                  .sel-#{fieldName f}
                                  value="#{fieldName f}"
                                  :not (fielddisp f):style="display:none">
                                  #{fieldTitle f}
                  <div .right-column-group>
                    <h6 .right-column-heading-leader>Random Sample
                    <div .sample-row>
                      <label for="sample">fraction
                      <input #sample
                        name="sample"
                        type="number"
                        step="any"
                        min="0"
                        max="1"
                        value="#{querySample query}"
                        title="Probability (0,1] with which to include each item"
                        onchange="sampleChange()">
                      <label for="seed">seed
                      <input #seed
                        name="seed"
                        type="number"
                        step="1"
                        min="0"
                        value="#{maybe mempty H.toMarkup $ querySeed query}"
                        :querySample query < 1:disabled=disabled
                        title="Random seed to generate sample selection"
                        onchange="sampleChange()">
                <div .tab-pane .fade #Python role="tabpanel" aria-labelledby="python-tab">
                  <div .right-column-group>
                    <h6 .right-column-heading>Python Query
                    <p>
                      Example python code to apply the above filters and retrieve data. To use, download and install
                      <a href="https://github.com/flatironinstitute/flathub/tree/prod/py">this module.
                    <div #div-py .python-block>
                      <pre #code-py>
                <div .tab-pane .fade #Fields role="tabpanel" aria-labelledby="dict-tab">
                  <div .right-column-group>
                    <h6 .right-column-heading>Fields Dictionary
                    <div>
                      <table #tdict .table .table-striped .table-sm>
                        <thead>
                          <tr>
                            <th>Field
                            <th>Variable
                            <th>Type
                            <th>Units
                            <th>Description
                        <tbody>
                          $forall f <- catalogFieldGroups cat
                            #{fielddesc f 0}
                <div .tab-pane .fade #About role="tabpanel" aria-labelledby="about-tab">
                  <div .right-column-group>
                    <h6 .right-column-heading>About Catalog
                    $forall desc <- catalogDescr cat
                      <p>#{H.preEscapedText desc}
                    $with gs <- findGroupsCatalog sim groups
                      $if not (null gs)
                        <div>
                          More under:
                          <ul>
                            $forall g <- gs
                              <li>
                                <a href="@{groupPage !:? g}">#{mintersperseMap "/" H.text g}

        <div .container-fluid .catalog-summary>
          <div .d-flex .justify-content-between>
            <div .d-flex>
              <button .btn .btn-warning #rawdata-btn
                type="button"
                onclick="toggleShowData()">
                Hide Raw Data
              <p .horizontal-label #info>
            <div #filt-tab .click-tab-container>
                <p .horizontal-label>Active Filters
                <div .click-tab
                  v-for="filter in filters"
                  v-bind:id="'filt-tab-'+filter.field.name">
                    <!-- TODO: Add back in ?
                    <div .click-tab-close $ mempty
                    -->
                    <p .click-tab-content
                      v-bind:field="filter.field"
                      v-bind:value="filter.field.value">
                      {{filter.field.name}}
            <!-- TODO: Fix this download function -->
            <div .download-container #download>
              <p .horizontal-label>Format
              <select v-model="bulk">
                <option value="">Choose format...
                <optgroup label="raw data">
                  $forall (n, f) <- HM.toList downloadFormats
                    <option #download.#{n} .download-option
                      value="@{apiRoute apiDownload :? (sim, f, Nothing)}">
                      #{n}
                    <option #download.#{n}.gz .download-option
                      value="@{apiRoute apiDownload :? (sim, f, Just CompressionGZip)}">
                      #{n}.gz
                $forall (l, f) <- [("" ++ "files", "zip"), ("download script", "sh")]
                  <optgroup label="attachment #{l}">
                    $with att <- HM.keys $ HM.filter (isJust . fieldAttachment) $ catalogFieldMap cat
                      $forall a <- att
                        <option #download.attachment.#{a}.#{f} .download-option
                          value="@{apiRoute apiAttachmentsField :? (sim, attachmentsFormats ! f, a)}">
                          #{a}.#{f}
                      $if not $ null att
                        <option #download.attachments.#{f} .download-option
                          value="@{apiRoute apiAttachments :? (sim, attachmentsFormats ! f)}">
                          all selected.#{f}
              <a .button .button-secondary #download-btn
                v-bind:href="link" download>
                Download

        <div .container-fluid .catalog-summary .raw-data #rawdata>
          <div .raw-data__header>
            <h5>Raw Data
          <table #tcat>
            <thead>
              #{toprow}
      |]


groupPage :: Route [T.Text]
groupPage = getPath ("group" R.*< R.manyI R.parameter) $ \path req -> do
  cats <- asks globalCatalogs
  grp <- maybe (raise404 (intercalate "/" (map T.unpack path) ++ " not found")) return $
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
                  $if all catalogVisible cat'
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
  cats <- maybe (raise404 $ intercalate "/" (map T.unpack path) ++ " not found") return .
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
                $forall cat <- catalogsSorted cats
                  <option value="#{catalogName cat}">
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
  fmod <- maybe (raise404 $ path ++ " not found") return =<<
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

agoraPage :: Route ()
agoraPage = getPath "agora" $ \() -> R.routeAction staticHtml ["agora"]

firePage :: Route ()
firePage = getPath "fire" $ \() -> R.routeAction staticHtml ["fire"]

-- backwards compatibility

sqlSchema :: Route Simulation
sqlSchema = getPath (R.parameter R.>* "schema.sql") $ \sim req ->
  return $ response permanentRedirect308 [ locationHeader req (apiRoute apiSchemaSQL) sim [] ] ()

csvSchema :: Route Simulation
csvSchema = getPath (R.parameter R.>* "schema.csv") $ \sim req ->
  return $ response permanentRedirect308 [ locationHeader req (apiRoute apiSchemaCSV) sim [] ] ()

attachment :: Route (Simulation, T.Text, T.Text)
attachment = getPath (R.parameter R.>* "attachment" R.>*<< R.parameter R.>*< R.parameter) $ \arg req ->
  return $ response permanentRedirect308 [ locationHeader req (apiRoute apiAttachment) arg [] ] ()
