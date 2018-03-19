{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Schema
  ( Type(..)
  , FieldSub(..)
  , Field, FieldGroup
  , Fields, FieldGroups
  , CatalogStore(..)
  , Catalog(..)
  , subField
  , expandFields
  , fieldsDepth
  , checkESIndices
  ) where

import           Control.Applicative ((<|>))
import           Control.Arrow ((&&&))
import           Control.Monad (forM_, unless)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import           Data.Default (Default(def))
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(Proxy))
import           Data.Semigroup (Max(getMax))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Text.Read (readPrec, Lexeme(Ident), lexP, readEither)

import JSON

data Type
  = Text
  | Keyword
  | Long
  | Integer
  | Short
  | Byte
  | Double
  | Float
  | HalfFloat
  | Date
  | Boolean
  | Binary
  deriving (Eq, Enum)

instance Default Type where
  def = Float

instance Show Type where
  show Text             = "text"
  show Keyword          = "keyword"
  show Long             = "long"
  show Integer          = "integer"
  show Short            = "short"
  show Byte             = "byte"
  show Double           = "double"
  show Float            = "float"
  show HalfFloat        = "half_float"
  show Date             = "date"
  show Boolean          = "boolean"
  show Binary           = "binary"

instance Read Type where
  readPrec = do
    Ident s <- lexP
    case s of
      "text"        -> return Text
      "keyword"     -> return Keyword
      "long"        -> return Long
      "integer"     -> return Integer
      "short"       -> return Short
      "byte"        -> return Byte
      "double"      -> return Double
      "float"       -> return Float
      "half_float"  -> return HalfFloat
      "date"        -> return Date
      "boolean"     -> return Boolean
      "binary"      -> return Binary
      _ -> fail "Unknown ES type"
      
instance J.ToJSON Type where
  toJSON = J.toJSON . show
  toEncoding = J.toEncoding . show

instance J.FromJSON Type where
  parseJSON = J.withText "ES type" $ either fail return . readEither . T.unpack
data FieldSub m = Field
  { fieldName :: T.Text
  , fieldType :: Type
  , fieldTitle :: T.Text
  , fieldDescr :: T.Text
  , fieldTop, fieldDisp :: Bool
  , fieldSub :: m (FieldsSub m)
  }

type FieldsSub m = V.Vector (FieldSub m)
type FieldGroup = FieldSub Maybe
type Field = FieldSub Proxy
type FieldGroups = FieldsSub Maybe
type Fields = [Field]

instance Default FieldGroup where
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

instance J.FromJSON FieldGroup where
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

subField :: FieldSub n -> FieldSub m -> FieldSub m
subField f s = s{ fieldName = fieldName f <> T.cons '_' (fieldName s) }

expandFields :: FieldGroups -> Fields
expandFields = foldMap expandField where
  expandField f@Field{ fieldSub = Nothing } = return f{ fieldSub = Proxy }
  expandField f@Field{ fieldSub = Just l } =
    foldMap (expandField . subField f) l

fieldsDepth :: FieldGroups -> Word
fieldsDepth = getMax . depth where
  depth = succ . foldMap (foldMap depth . fieldSub)

data CatalogStore
  = CatalogES
    { catalogIndex, catalogMapping :: T.Text
    }
  | CatalogPG
    { catalogTable :: T.Text
    }

data Catalog = Catalog
  { catalogTitle :: T.Text
  , catalogStore :: CatalogStore
  , catalogFields :: FieldGroups
  , catalogFieldMap :: HM.HashMap T.Text Field
  }

instance J.FromJSON Catalog where
  parseJSON = J.withObject "catalog" $ \c -> do
    f <- c J..: "fields"
    t <- c J..: "title"
    d <- CatalogES
        <$> (c J..: "index")
        <*> (c J..:! "mapping" J..!= "catalog")
      <|> CatalogPG
        <$> (c J..: "table")
    return $ Catalog t d f (HM.fromList $ map (fieldName &&& id) $ expandFields f)

checkESIndices :: [Catalog] -> J.Value -> J.Parser ()
checkESIndices cats = J.withObject "indices" $ forM_ cats . catalog where
  catalog is cat@Catalog{ catalogStore = CatalogES idxn mapn } = parseJSONField idxn (idx cat mapn) is
  catalog _ _ = return ()
  idx :: Catalog -> T.Text -> J.Value -> J.Parser ()
  idx cat mapn = J.withObject "index" $ parseJSONField "mappings" $ J.withObject "mappings" $
    parseJSONField mapn (mapping $ expandFields $ catalogFields cat)
  mapping :: Fields -> J.Value -> J.Parser ()
  mapping fields = J.withObject "mapping" $ parseJSONField "properties" $ J.withObject "properties" $ \ps ->
    forM_ fields $ \field -> parseJSONField (fieldName field) (prop field) ps
  prop :: Field -> J.Value -> J.Parser ()
  prop field = J.withObject "property" $ \p -> do
    t <- p J..: "type"
    unless (t == fieldType field) $ fail $ "incorrect field type; should be " ++ show (fieldType field)
