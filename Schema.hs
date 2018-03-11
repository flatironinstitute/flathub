{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Schema
  ( Field(..)
  , Fields
  , Catalog(..)
  , subField
  , expandFields
  , fieldsDepth
  , checkESIndices
  ) where

import           Control.Monad (forM_, unless)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import           Data.Default (Default(def))
import           Data.Monoid ((<>))
import           Data.Semigroup (Max(getMax))
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified ES.Types as ES
import JSON

data Field = Field
  { fieldName :: T.Text
  , fieldType :: ES.Type
  , fieldTitle :: T.Text
  , fieldDescr :: T.Text
  , fieldTop, fieldDisp :: Bool
  , fieldSub :: Maybe Fields
  }

type Fields = V.Vector Field

instance Default Field where
  def = Field T.empty def T.empty T.empty False True Nothing

instance J.ToJSON Field where
  toJSON Field{..} = J.object
    [ "name" J..= fieldName
    , "type" J..= fieldType
    , "title" J..= fieldTitle
    , "descr" J..= fieldDescr
    , "top" J..= fieldTop
    , "disp" J..= fieldDisp
    ]
  toEncoding Field{..} = J.pairs
    (  "name" J..= fieldName
    <> "type" J..= fieldType
    <> "title" J..= fieldTitle
    <> "descr" J..= fieldDescr
    <> "top" J..= fieldTop
    <> "disp" J..= fieldDisp
    )

instance J.FromJSON Field where
  parseJSON = parseFieldDefs def where
    parseFieldDefs d = J.withObject "field" $ \f -> do
      n <- f J..: "name"
      u <- f J..:? "units"
      r <- Field n
        <$> (f J..:! "type" J..!= fieldType d)
        <*> (f J..:! "title" J..!= n)
        <*> (maybe id (\u' -> (<> " (" <> u' <> ")")) u <$> f J..:? "descr" J..!= "")
        <*> (f J..:? "top" J..!= fieldTop d)
        <*> (f J..:! "disp" J..!= fieldDisp d)
        <*> return Nothing
      s <- J.explicitParseFieldMaybe' (J.withArray "subfields" $ V.mapM $ parseFieldDefs r) f "sub"
      return r{ fieldSub = s }

subField :: Field -> Field -> Field
subField f s = s{ fieldName = fieldName f <> T.cons '_' (fieldName s) }

expandFields :: Fields -> Fields
expandFields = foldMap expandField where
  expandField f@Field{ fieldSub = Nothing } = return f
  expandField f@Field{ fieldSub = Just l } =
    foldMap (expandField . subField f) l

fieldsDepth :: Fields -> Word
fieldsDepth = getMax . depth where
  depth = succ . foldMap (foldMap depth . fieldSub)

data Catalog = Catalog
  { catalogTitle :: T.Text
  , catalogIndex, catalogMapping :: T.Text
  , catalogFields :: Fields
  }

instance J.FromJSON Catalog where
  parseJSON = J.withObject "catalog" $ \c ->
    Catalog
      <$> (c J..: "title")
      <*> (c J..: "index")
      <*> (c J..:! "mapping" J..!= "catalog")
      <*> (c J..: "fields")

checkESIndices :: [Catalog] -> J.Value -> J.Parser ()
checkESIndices cats = J.withObject "indices" $ \is ->
  forM_ cats $ \cat -> parseJSONField (catalogIndex cat) (idx cat) is
  where
  idx :: Catalog -> J.Value -> J.Parser ()
  idx cat = J.withObject "index" $ parseJSONField "mappings" $ J.withObject "mappings" $
    parseJSONField (catalogMapping cat) (mapping $ expandFields $ catalogFields cat)
  mapping :: Fields -> J.Value -> J.Parser ()
  mapping fields = J.withObject "mapping" $ parseJSONField "properties" $ J.withObject "properties" $ \ps ->
    forM_ fields $ \field -> parseJSONField (fieldName field) (prop field) ps
  prop :: Field -> J.Value -> J.Parser ()
  prop field = J.withObject "property" $ \p -> do
    t <- p J..: "type"
    unless (t == fieldType field) $ fail $ "incorrect field type; should be " ++ show (fieldType field)
