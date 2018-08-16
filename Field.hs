{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Field
  ( TypeValue(..)
  , Type, Value
  , typeValue, typeValue1
  , fmapTypeValue, fmapTypeValue1
  , typeOfValue
  , unTypeValue
  , parseTypeValue
  , parseTypeJSONValue
  , baseType
  , typeIsFloating, typeIsIntegral, typeIsNumeric, typeIsString
  , numpySize, numpyDtype
  , FieldSub(..)
  , Field, FieldGroup
  , Fields, FieldGroups
  , parseFieldGroup
  , expandField, expandFields, expandAllFields
  , deleteField
  , fieldsDepth
  , FieldValue
  , parseFieldValue
  , isTermsField
  ) where

import           Control.Applicative (Alternative, empty, (<|>))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import           Data.Default (Default(def))
import           Data.Functor.Classes (Eq1, eq1, Show1, showsPrec1)
import           Data.Functor.Identity (Identity(Identity, runIdentity))
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64, Int32, Int16, Int8)
import           Data.Proxy (Proxy(Proxy))
import           Data.Semigroup (Max(getMax), Semigroup((<>)))
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import qualified Data.Vector as V
import           Numeric.Half (Half)
import           Text.Read (readMaybe, readPrec, Lexeme(Ident), lexP, readEither)

import Monoid

instance J.ToJSON Half where
  toJSON = J.toJSON . (realToFrac :: Half -> Float)
  toEncoding = J.toEncoding . (realToFrac :: Half -> Float)

instance J.FromJSON Half where
  parseJSON = fmap (realToFrac :: Float -> Half) . J.parseJSON

data TypeValue f
  = Double    !(f Double)
  | Float     !(f Float)
  | HalfFloat !(f Half)
  | Long      !(f Int64)
  | Integer   !(f Int32)
  | Short     !(f Int16)
  | Byte      !(f Int8)
  | Boolean   !(f Bool)
  | Text      !(f T.Text)
  | Keyword   !(f T.Text)

type Type = TypeValue Proxy
type Value = TypeValue Identity

class (Eq a, Ord a, Show a, Read a, J.ToJSON a, J.FromJSON a, Typeable a) => Typed a where
  typeValue :: f a -> TypeValue f

typeValue1 :: Typed a => a -> Value
typeValue1 = typeValue . Identity

instance Typed T.Text where typeValue = Text
instance Typed Int64  where typeValue = Long
instance Typed Int32  where typeValue = Integer
instance Typed Int16  where typeValue = Short
instance Typed Int8   where typeValue = Byte
instance Typed Double where typeValue = Double
instance Typed Float  where typeValue = Float
instance Typed Half   where typeValue = HalfFloat
instance Typed Bool   where typeValue = Boolean

transformTypeValue :: Applicative g => (forall a . Typed a => f a -> g (h a)) -> TypeValue f -> g (TypeValue h)
transformTypeValue f (Double    x) = Double    <$> f x
transformTypeValue f (Float     x) = Float     <$> f x
transformTypeValue f (HalfFloat x) = HalfFloat <$> f x
transformTypeValue f (Long      x) = Long      <$> f x
transformTypeValue f (Integer   x) = Integer   <$> f x
transformTypeValue f (Short     x) = Short     <$> f x
transformTypeValue f (Byte      x) = Byte      <$> f x
transformTypeValue f (Boolean   x) = Boolean   <$> f x
transformTypeValue f (Text      x) = Text      <$> f x
transformTypeValue f (Keyword   x) = Keyword   <$> f x

_traverseTypeValue :: Applicative g => (forall a . Typed a => f a -> g a) -> TypeValue f -> g Value
_traverseTypeValue f = transformTypeValue (fmap Identity . f)

sequenceTypeValue :: Applicative f => TypeValue f -> f Value
sequenceTypeValue = transformTypeValue (fmap Identity)

-- isn't there a Functor1 class for this or something?
fmapTypeValue :: (forall a . Typed a => f a -> g a) -> TypeValue f -> TypeValue g
fmapTypeValue f = runIdentity . transformTypeValue (Identity . f)

fmapTypeValue1 :: (forall a . Typed a => f a -> a) -> TypeValue f -> Value
fmapTypeValue1 f = fmapTypeValue (Identity . f)

unTypeValue :: (forall a . Typed a => f a -> b) -> TypeValue f -> b
unTypeValue f (Double    x) = f x
unTypeValue f (Float     x) = f x
unTypeValue f (HalfFloat x) = f x
unTypeValue f (Long      x) = f x
unTypeValue f (Integer   x) = f x
unTypeValue f (Short     x) = f x
unTypeValue f (Byte      x) = f x
unTypeValue f (Boolean   x) = f x
unTypeValue f (Text      x) = f x
unTypeValue f (Keyword   x) = f x

typeOfValue :: TypeValue f -> Type
typeOfValue = fmapTypeValue (const Proxy)

parseTypeValue :: Type -> T.Text -> TypeValue Maybe
parseTypeValue (Text    _) s       = Text    $ Just s
parseTypeValue (Keyword _) s       = Keyword $ Just s
parseTypeValue (Boolean _) "0"     = Boolean $ Just False
parseTypeValue (Boolean _) "false" = Boolean $ Just False
parseTypeValue (Boolean _) "1"     = Boolean $ Just True
parseTypeValue (Boolean _) "true"  = Boolean $ Just True
parseTypeValue t s = fmapTypeValue (\Proxy -> readMaybe $ T.unpack s) t

instance Eq1 f => Eq (TypeValue f) where
  Double x == Double y = eq1 x y
  Float x == Float y = eq1 x y
  HalfFloat x == HalfFloat y = eq1 x y
  Long x == Long y = eq1 x y
  Integer x == Integer y = eq1 x y
  Short x == Short y = eq1 x y
  Byte x == Byte y = eq1 x y
  Boolean x == Boolean y = eq1 x y
  Text x == Text y = eq1 x y
  Keyword x == Keyword y = eq1 x y
  _ == _ = False

instance {-# OVERLAPPABLE #-} Show1 f => Show (TypeValue f) where
  showsPrec i = unTypeValue (showsPrec1 i)

instance {-# OVERLAPPABLE #-} J.ToJSON1 f => J.ToJSON (TypeValue f) where
  toJSON = unTypeValue J.toJSON1
  toEncoding = unTypeValue J.toEncoding1

parseTypeJSONValue :: Type -> J.Value -> TypeValue Maybe
parseTypeJSONValue t (J.String s) = parseTypeValue t s
parseTypeJSONValue t j = fmapTypeValue (\Proxy -> J.parseMaybe J.parseJSON j) t

instance Default Type where
  def = Float Proxy

instance {-# OVERLAPPING #-} Show Type where
  show (Text _)      = "text"
  show (Keyword _)   = "keyword"
  show (Long _)      = "long"
  show (Integer _)   = "integer"
  show (Short _)     = "short"
  show (Byte _)      = "byte"
  show (Double _)    = "double"
  show (Float _)     = "float"
  show (HalfFloat _) = "half_float"
  show (Boolean _)   = "boolean"

instance Read Type where
  readPrec = do
    Ident s <- lexP
    case s of
      "text"        -> return (Text Proxy)
      "string"      -> return (Text Proxy)
      "keyword"     -> return (Keyword Proxy)
      "long"        -> return (Long Proxy)
      "int"         -> return (Integer Proxy)
      "integer"     -> return (Integer Proxy)
      "short"       -> return (Short Proxy)
      "byte"        -> return (Byte Proxy)
      "double"      -> return (Double Proxy)
      "float"       -> return (Float Proxy)
      "half_float"  -> return (HalfFloat Proxy)
      "bool"        -> return (Boolean Proxy)
      "boolean"     -> return (Boolean Proxy)
      _ -> fail "Unknown type"
      
instance {-# OVERLAPPING #-} J.ToJSON Type where
  toJSON = J.toJSON . show
  toEncoding = J.toEncoding . show

instance J.FromJSON Type where
  parseJSON = J.withText "type" $ either fail return . readEither . T.unpack

typeIsFloating :: Type -> Bool
typeIsFloating (Double    _) = True
typeIsFloating (Float     _) = True
typeIsFloating (HalfFloat _) = True
typeIsFloating _ = False

typeIsIntegral :: Type -> Bool
typeIsIntegral (Long      _) = True
typeIsIntegral (Integer   _) = True
typeIsIntegral (Short     _) = True
typeIsIntegral (Byte      _) = True
typeIsIntegral _ = False

typeIsNumeric :: Type -> Bool
typeIsNumeric t = typeIsFloating t || typeIsIntegral t

typeIsString :: Type -> Bool
typeIsString (Keyword   _) = True
typeIsString (Text      _) = True
typeIsString _ = False

baseType :: (a,a,a,a) -> Type -> a
baseType (f,i,_,s) t
  | typeIsFloating t = f
  | typeIsIntegral t = i
  | typeIsString   t = s
baseType (_,_,b,_) ~(Boolean   _) = b

numpySize :: Type -> Word
numpySize (Double    _) = 8
numpySize (Float     _) = 4
numpySize (HalfFloat _) = 2
numpySize (Long      _) = 8
numpySize (Integer   _) = 4
numpySize (Short     _) = 2
numpySize (Byte      _) = 1
numpySize (Boolean   _) = 1
numpySize (Keyword   _) = 8
numpySize (Text      _) = 16

numpyDtype :: Type -> String
numpyDtype (Boolean _) = "?"
numpyDtype t = '<' : baseType ('f','i','?','S') t : show (numpySize t) where

data FieldSub t m = Field
  { fieldName :: T.Text
  , fieldType :: TypeValue t
  , fieldEnum :: Maybe (V.Vector T.Text)
  , fieldTitle :: T.Text
  , fieldDescr :: Maybe T.Text
  , fieldUnits :: Maybe T.Text
  , fieldTop, fieldDisp :: Bool
  , fieldSub :: m (FieldsSub t m)
  , fieldDict :: Maybe T.Text
  }

type FieldGroup = FieldSub Proxy Maybe
type Field = FieldSub Proxy Proxy
type FieldValue = FieldSub Identity Proxy

type FieldsSub t m = V.Vector (FieldSub t m)
type FieldGroups = FieldsSub Proxy Maybe
type Fields = [Field]

instance Alternative m => Default (FieldSub Proxy m) where
  def = Field
    { fieldName = T.empty
    , fieldType = def
    , fieldEnum = Nothing
    , fieldTitle = T.empty
    , fieldDescr = Nothing
    , fieldUnits = Nothing
    , fieldTop = False
    , fieldDisp = True
    , fieldSub = empty
    , fieldDict = Nothing
    }

instance J.ToJSON Field where
  toJSON f@Field{..} = J.object
    [ "name" J..= fieldName
    , "type" J..= fieldType
    , "enum" J..= fieldEnum
    , "title" J..= fieldTitle
    , "descr" J..= fieldDescr
    , "units" J..= fieldUnits
    , "top" J..= fieldTop
    , "disp" J..= fieldDisp
    , "dtype" J..= numpyDtype fieldType
    , "terms" J..= isTermsField f
    , "base" J..= baseType ('f','i','b','s') fieldType
    , "dict" J..= fieldDict
    ]

parseFieldGroup :: HM.HashMap T.Text FieldGroup -> J.Value -> J.Parser FieldGroup
parseFieldGroup dict = parseFieldDefs def where
  parseFieldDefs :: FieldGroup -> J.Value -> J.Parser FieldGroup
  parseFieldDefs defd = J.withObject "field" $ \f -> do
    fieldDict <- f J..:? "dict"
    d <- maybe (return defd)
      (\n -> maybe (fail $ "Unknown dict key: " ++ show n) return $ HM.lookup n dict)
      fieldDict
    fieldName <- f J..:? "name" J..!= fieldName d
    fieldType <- f J..:! "type" J..!= fieldType d
    fieldEnum <- (<|> fieldEnum d) <$> f J..:? "enum"
    fieldTitle <- f J..:! "title" J..!= if T.null (fieldTitle d) then fieldName else fieldTitle d
    fieldDescr <- (<|> fieldDescr d) <$> f J..:? "descr"
    fieldUnits <- (<|> fieldUnits d) <$> f J..:? "units"
    fieldTop <- f J..:? "top" J..!= fieldTop d
    fieldDisp <- f J..:! "disp" J..!= fieldDisp d
    fieldSub <- (<|> fieldSub d) <$> J.explicitParseFieldMaybe' (J.withArray "subfields" $ V.mapM $
        parseFieldDefs defd
          { fieldType = fieldType
          , fieldEnum = fieldEnum
          , fieldTop = fieldTop
          , fieldDisp = fieldDisp
          })
      f "sub"
    return Field{..}

instance J.FromJSON FieldGroup where
  parseJSON = parseFieldGroup mempty

instance Semigroup (FieldSub t m) where
  (<>) = subField

instance Alternative m => Monoid (FieldSub Proxy m) where
  mempty = def
  mappend = subField

subField :: FieldSub s n -> FieldSub t m -> FieldSub t m
subField f s = s
  { fieldName = merge '_' (fieldName f) (fieldName s)
  , fieldTitle = merge ' ' (fieldTitle f) (fieldTitle s)
  , fieldDescr = joinMaybeWith (\x -> (x <>) . T.cons '\n') (fieldDescr f) (fieldDescr s)
  , fieldUnits = fieldUnits s <|> fieldUnits f
  } where
  merge c a b
    | T.null a = b
    | T.null b = a
    | a == b = b
    | otherwise = a <> T.cons c b

expandField :: FieldGroup -> Fields
expandField f@Field{ fieldSub = Nothing } = return f{ fieldSub = Proxy }
expandField f@Field{ fieldSub = Just l } =
  foldMap (expandField . mappend f) l

expandFields :: FieldGroups -> Fields
expandFields = foldMap expandField

expandAllFields :: FieldGroups -> HM.HashMap T.Text FieldGroup
expandAllFields = foldMap expandAllField where
  expandAllField :: FieldGroup -> HM.HashMap T.Text FieldGroup
  expandAllField f = HM.singleton (fieldName f) f <> foldMap (foldMap (expandAllField . mappend f)) (fieldSub f)

deleteField :: T.Text -> FieldGroups -> FieldGroups
deleteField n = dfs mempty where
  df p f@Field{ fieldSub = Nothing }
    | n == fieldName (p <> f) = Nothing
    | otherwise = Just f
  df p f@Field{ fieldSub = Just l } = Just f{ fieldSub = Just $ dfs (p <> f) l }
  dfs = V.mapMaybe . df

fieldsDepth :: FieldGroups -> Word
fieldsDepth = getMax . depth where
  depth = succ . foldMap (foldMap depth . fieldSub)

parseFieldValue :: Field -> T.Text -> Maybe FieldValue
parseFieldValue f = fmap sv . pv f where
  pv Field{ fieldType = (Byte _), fieldEnum = Just l } s | Just i <- V.elemIndex s l = Just $ Byte $ fromIntegral i
  pv Field{ fieldType = t } s = sequenceTypeValue $ parseTypeValue t s
  sv v = f
    { fieldType = v
    , fieldSub = Proxy
    }

isTermsField :: Field -> Bool
isTermsField Field{ fieldType = Keyword _ } = True
isTermsField Field{ fieldType = Text _ } = True
isTermsField Field{ fieldType = Byte _, fieldEnum = Just _ } = True
isTermsField Field{ fieldType = Byte _, fieldTop = True } = True
isTermsField _ = False
