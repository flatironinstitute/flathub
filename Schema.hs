{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Schema
  ( Field(..)
  , Catalog(..)
  , checkESIndices
  ) where

import           Control.Monad (forM_, unless)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import           Data.Monoid ((<>))
import qualified Data.Text as T

import qualified ES.Types as ES
import JSON

data Field = Field
  { fieldName :: T.Text
  , fieldType :: ES.Type
  , fieldTitle :: T.Text
  , fieldDescr :: T.Text
  , fieldTop, fieldDisp :: Bool
  }

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
  parseJSON = J.withObject "field" $ \f -> do
    n <- f J..: "name"
    u <- f J..:? "units"
    Field n
      <$> (f J..: "type")
      <*> (f J..:! "title" J..!= n)
      <*> (maybe id (\u' -> (<> " (" <> u' <> ")")) u <$> f J..:? "descr" J..!= "")
      <*> (f J..:? "top" J..!= False)
      <*> (f J..:! "disp" J..!= True)

data Catalog = Catalog
  { catalogTitle :: T.Text
  , catalogIndex, catalogMapping :: T.Text
  , catalogFields :: [Field]
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
    parseJSONField (catalogMapping cat) (mapping $ catalogFields cat)
  mapping :: [Field] -> J.Value -> J.Parser ()
  mapping fields = J.withObject "mapping" $ parseJSONField "properties" $ J.withObject "properties" $ \ps ->
    forM_ fields $ \field -> parseJSONField (fieldName field) (prop field) ps
  prop :: Field -> J.Value -> J.Parser ()
  prop field = J.withObject "property" $ \p -> do
    t <- p J..: "type"
    unless (t == fieldType field) $ fail $ "incorrect field type; should be " ++ show (fieldType field)
