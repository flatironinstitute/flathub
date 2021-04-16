{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Type
  ( TypeValue(..)
  , Type, Value
  , Typed
  , typeValue, typeValue1
  , traverseTypeValue
  , sequenceTypeValue
  , fmapTypeValue, fmapTypeValue1, fmapTypeValue2
  , typeOfValue
  , unTypeValue
  , parseTypeValue
  , parseJSONTypeValue
  , parseTypeJSONValue
  , baseType
  , typeIsFloating, typeIsIntegral, typeIsNumeric, typeIsString, typeIsBoolean
  , numpySize, numpyDtype
  , sqlType
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import           Data.Default (Default(def))
import           Data.Functor.Classes (Eq1, eq1, Show1, showsPrec1)
import           Data.Functor.Const (Const(Const, getConst))
import           Data.Functor.Identity (Identity(Identity, runIdentity))
import           Data.Int (Int64, Int32, Int16, Int8)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Numeric.Half (Half)
import           Text.Read (readMaybe, readPrec, Lexeme(Ident), lexP, readEither)

instance J.ToJSON Half where
  toJSON = J.toJSON . (realToFrac :: Half -> Float)
  toEncoding = J.toEncoding . (realToFrac :: Half -> Float)

instance J.FromJSON Half where
  parseJSON = fmap (realToFrac :: Float -> Half) . J.parseJSON

instance Enum Half where
  succ = (+) 1
  pred = (-) 1
  toEnum = realToFrac
  fromEnum = truncate
  enumFrom x = x : enumFrom (succ x)
  enumFromThen x y = x : f y where
    d = y - x
    f a = a : f (a + d)
  enumFromTo x z
    | z >= x = x : enumFromTo (succ x) z
    | otherwise = []
  enumFromThenTo x y z
    | c x = x : f y
    | otherwise = [] where
    f a
      | c a = a : f (a + d)
      | otherwise = []
    d = y - x
    c | d > 0 = (z >=)
      | otherwise = (z <=)

data TypeValue f
  = Double    !(f Double)
  | Float     !(f Float)
  | HalfFloat !(f Half)
  | Long      !(f Int64)
  | Integer   !(f Int32)
  | Short     !(f Int16)
  | Byte      !(f Int8)
  | Boolean   !(f Bool)
  | Void      !(f ())
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
instance Typed ()     where typeValue = Void

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
unTypeValue f (Void      x) = f x

transformTypeValue :: Functor g => (forall a . Typed a => f a -> g (h a)) -> TypeValue f -> g (TypeValue h)
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
transformTypeValue f (Void      x) = Void      <$> f x

transformTypeValue2 :: Monad g => (forall a . Typed a => f a -> f a -> g (h a)) -> TypeValue f -> TypeValue f -> g (TypeValue h)
transformTypeValue2 f (Double    x) (Double    y) = Double    <$> f x y
transformTypeValue2 f (Float     x) (Float     y) = Float     <$> f x y
transformTypeValue2 f (HalfFloat x) (HalfFloat y) = HalfFloat <$> f x y
transformTypeValue2 f (Long      x) (Long      y) = Long      <$> f x y
transformTypeValue2 f (Integer   x) (Integer   y) = Integer   <$> f x y
transformTypeValue2 f (Short     x) (Short     y) = Short     <$> f x y
transformTypeValue2 f (Byte      x) (Byte      y) = Byte      <$> f x y
transformTypeValue2 f (Boolean   x) (Boolean   y) = Boolean   <$> f x y
transformTypeValue2 f (Text      x) (Text      y) = Text      <$> f x y
transformTypeValue2 f (Keyword   x) (Keyword   y) = Keyword   <$> f x y
transformTypeValue2 f (Void      x) (Void      y) = Void      <$> f x y
transformTypeValue2 _ _             _             = error "transformTypeValue2: type mismatch"

traverseTypeValue :: Functor g => (forall a . Typed a => f a -> g a) -> TypeValue f -> g Value
traverseTypeValue f = transformTypeValue (fmap Identity . f)

sequenceTypeValue :: Functor f => TypeValue f -> f Value
sequenceTypeValue = transformTypeValue (fmap Identity)

-- isn't there a Functor1 class for this or something?
fmapTypeValue :: (forall a . Typed a => f a -> g a) -> TypeValue f -> TypeValue g
fmapTypeValue f = runIdentity . transformTypeValue (Identity . f)

fmapTypeValue1 :: (forall a . Typed a => f a -> a) -> TypeValue f -> Value
fmapTypeValue1 f = fmapTypeValue (Identity . f)

fmapTypeValue2 :: (forall a . Typed a => f a -> f a -> g a) -> TypeValue f -> TypeValue f -> TypeValue g
fmapTypeValue2 f = (runIdentity .) . transformTypeValue2 ((Identity .) . f)

typeOfValue :: TypeValue f -> Type
typeOfValue = fmapTypeValue (const Proxy)

instance Eq1 f => Eq (TypeValue f) where
  a == b = maybe False (unTypeValue getConst) $ transformTypeValue2 (\x y -> Just $ Const $ eq1 x y) a b

instance {-# OVERLAPPABLE #-} Show1 f => Show (TypeValue f) where
  showsPrec i = unTypeValue (showsPrec1 i)

instance Show Value where
  showsPrec i = unTypeValue (showsPrec i . runIdentity)

parseTypeValue :: Type -> T.Text -> TypeValue Maybe
parseTypeValue (Text    _) s       = Text    $ Just s
parseTypeValue (Keyword _) s       = Keyword $ Just s
parseTypeValue (Boolean _) "0"     = Boolean $ Just False
parseTypeValue (Boolean _) "false" = Boolean $ Just False
parseTypeValue (Boolean _) "1"     = Boolean $ Just True
parseTypeValue (Boolean _) "true"  = Boolean $ Just True
parseTypeValue t s = fmapTypeValue (\Proxy -> readMaybe $ T.unpack s) t

instance {-# OVERLAPPABLE #-} J.ToJSON1 f => J.ToJSON (TypeValue f) where
  toJSON = unTypeValue J.toJSON1
  toEncoding = unTypeValue J.toEncoding1

parseJSONTypeValue :: Type -> J.Value -> J.Parser Value
parseJSONTypeValue t j = traverseTypeValue (\Proxy -> J.parseJSON j) t

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
  show (Void _)      = "void"

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
      "int32"       -> return (Integer Proxy)
      "short"       -> return (Short Proxy)
      "int16"       -> return (Short Proxy)
      "byte"        -> return (Byte Proxy)
      "int8"        -> return (Byte Proxy)
      "double"      -> return (Double Proxy)
      "float8"      -> return (Double Proxy)
      "float"       -> return (Float Proxy)
      "float4"      -> return (Float Proxy)
      "half_float"  -> return (HalfFloat Proxy)
      "float2"      -> return (HalfFloat Proxy)
      "bool"        -> return (Boolean Proxy)
      "boolean"     -> return (Boolean Proxy)
      "void"        -> return (Void Proxy)
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

typeIsBoolean :: Type -> Bool
typeIsBoolean (Boolean _) = True
typeIsBoolean _ = False

baseType :: (a,a,a,a,a) -> Type -> a
baseType (f,i,b,s,_) t
  | typeIsFloating t = f
  | typeIsIntegral t = i
  | typeIsString   t = s
  | typeIsBoolean  t = b
baseType (_,_,_,_,v) ~(Void _) = v

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
numpySize (Void      _) = 0

numpyDtype :: Type -> String
numpyDtype (Boolean _) = "?"
numpyDtype t = '<' : baseType ('f','i','?','S','V') t : show (numpySize t) where

sqlType :: Type -> T.Text
sqlType (Text _)      = "text"
sqlType (Keyword _)   = "text"
sqlType (Long _)      = "bigint"
sqlType (Integer _)   = "integer"
sqlType (Short _)     = "smallint"
sqlType (Byte _)      = "smallint"
sqlType (Double _)    = "double precision"
sqlType (Float _)     = "real"
sqlType (HalfFloat _) = "real"
sqlType (Boolean _)   = "boolean"
sqlType (Void _)      = "void"
