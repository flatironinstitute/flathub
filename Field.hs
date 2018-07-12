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
  , onTypeValue
  , parseTypeJSONValue
  , numpySize, numpyDtype
  , FieldSub(..)
  , Field, FieldGroup
  , Fields, FieldGroups
  , subField
  , expandFields
  , fieldsDepth
  ) where

import           Control.Applicative ((<|>))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import           Data.Default (Default(def))
import           Data.Functor.Classes (Eq1, eq1, Show1, showsPrec1)
import           Data.Functor.Identity (Identity(Identity))
import           Data.Int (Int64, Int32, Int16, Int8)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(Proxy))
import           Data.Semigroup (Max(getMax))
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

-- isn't there a Functor1 class for this or something?
fmapTypeValue :: (forall a . Typed a => f a -> g a) -> TypeValue f -> TypeValue g
fmapTypeValue f (Double    x) = Double    $ f x
fmapTypeValue f (Float     x) = Float     $ f x
fmapTypeValue f (HalfFloat x) = HalfFloat $ f x
fmapTypeValue f (Long      x) = Long      $ f x
fmapTypeValue f (Integer   x) = Integer   $ f x
fmapTypeValue f (Short     x) = Short     $ f x
fmapTypeValue f (Byte      x) = Byte      $ f x
fmapTypeValue f (Boolean   x) = Boolean   $ f x
fmapTypeValue f (Text      x) = Text      $ f x
fmapTypeValue f (Keyword   x) = Keyword   $ f x

fmapTypeValue1 :: (forall a . Typed a => f a -> a) -> TypeValue f -> Value
fmapTypeValue1 f = fmapTypeValue (Identity . f)

onTypeValue :: (forall a . Typed a => f a -> b) -> TypeValue f -> b
onTypeValue f (Double    x) = f x
onTypeValue f (Float     x) = f x
onTypeValue f (HalfFloat x) = f x
onTypeValue f (Long      x) = f x
onTypeValue f (Integer   x) = f x
onTypeValue f (Short     x) = f x
onTypeValue f (Byte      x) = f x
onTypeValue f (Boolean   x) = f x
onTypeValue f (Text      x) = f x
onTypeValue f (Keyword   x) = f x

typeOfValue :: TypeValue f -> Type
typeOfValue = fmapTypeValue (const Proxy)

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
  showsPrec i = onTypeValue (showsPrec1 i)

instance {-# OVERLAPPABLE #-} J.ToJSON1 f => J.ToJSON (TypeValue f) where
  toJSON = onTypeValue J.toJSON1
  toEncoding = onTypeValue J.toEncoding1

parseTypeJSONValue :: Type -> J.Value -> TypeValue Maybe
parseTypeJSONValue t j = fmapTypeValue (\Proxy -> J.parseMaybe J.parseJSON j
  <|> case j of { J.String s -> readMaybe (T.unpack s) ; _ -> Nothing }) t

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
numpyDtype t = '<' : numpyBtype t : show (numpySize t) where
  numpyBtype (Double    _) = 'f'
  numpyBtype (Float     _) = 'f'
  numpyBtype (HalfFloat _) = 'f'
  numpyBtype (Long      _) = 'i'
  numpyBtype (Integer   _) = 'i'
  numpyBtype (Short     _) = 'i'
  numpyBtype (Byte      _) = 'i'
  numpyBtype (Boolean   _) = '?'
  numpyBtype (Keyword   _) = 'S'
  numpyBtype (Text      _) = 'S'

data FieldSub m = Field
  { fieldName :: T.Text
  , fieldType :: Type
  , fieldTitle :: T.Text
  , fieldDescr :: Maybe T.Text
  , fieldUnits :: Maybe T.Text
  , fieldTop, fieldDisp :: Bool
  , fieldSub :: m (FieldsSub m)
  }

type FieldsSub m = V.Vector (FieldSub m)
type FieldGroup = FieldSub Maybe
type Field = FieldSub Proxy
type FieldGroups = FieldsSub Maybe
type Fields = [Field]

instance Default FieldGroup where
  def = Field
    { fieldName = T.empty
    , fieldType = def
    , fieldTitle = T.empty
    , fieldDescr = Nothing
    , fieldUnits = Nothing
    , fieldTop = False
    , fieldDisp = True
    , fieldSub = Nothing
    }

instance J.ToJSON Field where
  toJSON Field{..} = J.object
    [ "name" J..= fieldName
    , "type" J..= fieldType
    , "title" J..= fieldTitle
    , "descr" J..= fieldDescr
    , "units" J..= fieldUnits
    , "top" J..= fieldTop
    , "disp" J..= fieldDisp
    , "dtype" J..= numpyDtype fieldType
    ]
  toEncoding Field{..} = J.pairs
    (  "name" J..= fieldName
    <> "type" J..= fieldType
    <> "title" J..= fieldTitle
    <> "descr" J..= fieldDescr
    <> "units" J..= fieldUnits
    <> "top" J..= fieldTop
    <> "disp" J..= fieldDisp
    <> "dtype" J..= numpyDtype fieldType
    )

instance J.FromJSON FieldGroup where
  parseJSON = parseFieldDefs def where
    parseFieldDefs d = J.withObject "field" $ \f -> do
      n <- f J..: "name"
      r <- Field n
        <$> (f J..:! "type" J..!= fieldType d)
        <*> (f J..:! "title" J..!= n)
        <*> (f J..:? "descr")
        <*> (f J..:? "units")
        <*> (f J..:? "top" J..!= fieldTop d)
        <*> (f J..:! "disp" J..!= fieldDisp d)
        <*> return Nothing
      s <- J.explicitParseFieldMaybe' (J.withArray "subfields" $ V.mapM $ parseFieldDefs r) f "sub"
      return r{ fieldSub = s }

subField :: FieldSub n -> FieldSub m -> FieldSub m
subField f s = s
  { fieldName = fieldName f <> T.cons '_' (fieldName s)
  , fieldTitle = fieldTitle f <> T.cons ' ' (fieldTitle s)
  , fieldDescr = joinMaybeWith (\x -> (x <>) . T.cons '\n') (fieldDescr f) (fieldDescr s)
  }

expandFields :: FieldGroups -> Fields
expandFields = foldMap expandField where
  expandField f@Field{ fieldSub = Nothing } = return f{ fieldSub = Proxy }
  expandField f@Field{ fieldSub = Just l } =
    foldMap (expandField . subField f) l

fieldsDepth :: FieldGroups -> Word
fieldsDepth = getMax . depth where
  depth = succ . foldMap (foldMap depth . fieldSub)

