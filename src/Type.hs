{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Type
  ( TypeValue(..)
  , Type, Value
  , Void
  , scalarTypes
  , Typed
  , typeValue, typeValue1
  , traverseValue
  , sequenceValue
  , fmapValue
  , traverseTypeValue, traverseTypeValue2
  , fmapTypeValue, fmapTypeValue1, fmapTypeValue2
  , makeTypeValue, makeTypeValueM
  , coerceTypeValue
  , TypeTraversable(..)
  , typeOfValue
  , unTypeValue
  , toDouble
  , fromInt
  , readValue
  , renderValue
  , parseJSONValue
  , parseJSONOrStringValue
  , parseStream
  , baseType
  , arrayHead
  , singletonArray
  , typeIsArray
  , unArrayType
  , typeIsFloating, typeIsIntegral, typeIsNumeric, typeIsString, typeIsBoolean
  , numpyTypeSize
  ) where

import           Control.Applicative ((<|>), many, empty)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import           Data.Default (Default(def))
import           Data.Functor.Classes (Eq1, eq1, Show1, showsPrec1)
import           Data.Functor.Const (Const(..))
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(Identity, runIdentity))
import           Data.Int (Int64, Int32, Int16, Int8)
import qualified Data.JsonStream.Parser as JS
import           Data.Maybe (fromMaybe)
import qualified Data.OpenApi as OA
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           Data.Typeable (Typeable)
import qualified Data.Vector as V
import           Data.Void (Void, absurd, vacuous)
import           Data.Word (Word64)
import           Text.Read (readMaybe, readPrec, Lexeme(Ident), lexP, readEither)

import Half

instance OA.ToParamSchema Void where
  toParamSchema _ = mempty
    { OA._schemaTitle = Just "void"
    , OA._schemaNot = Just (OA.Inline mempty)
    }

instance OA.ToSchema Void where
  declareNamedSchema = return . OA.NamedSchema Nothing . OA.paramSchemaToSchema

data TypeValue f
  = Double    !(f Double)
  | Float     !(f Float)
  | HalfFloat !(f Half)
  | Long      !(f Int64)
  | ULong     !(f Word64)
  | Integer   !(f Int32)
  | Short     !(f Int16)
  | Byte      !(f Int8)
  | Boolean   !(f Bool)
  | Keyword   !(f T.Text)
  | Array (TypeValue (Compose f V.Vector))
  | Void      !(f Void)

type Type = TypeValue Proxy
type Value = TypeValue Identity

scalarTypes :: [Type]
scalarTypes =
  [ Double    Proxy
  , Float     Proxy
  , HalfFloat Proxy
  , Long      Proxy
  , ULong     Proxy
  , Integer   Proxy
  , Short     Proxy
  , Byte      Proxy
  , Boolean   Proxy
  , Keyword   Proxy
  ]

class (Eq a, Ord a, Show a, Read a, J.ToJSON a, J.FromJSON a, OA.ToParamSchema a, OA.ToSchema a, Typeable a) => Typed a where
  typeValue :: f a -> TypeValue f
  toDouble :: a -> Double
  fromInt :: Int -> a
  default fromInt :: Num a => Int -> a
  fromInt = fromIntegral
  -- |Certain representations ES uses do not always match what you expect, see we need a more permissive parser in some cases
  readValue :: BS.ByteString -> Maybe a
  readValue = readMaybe . BSC.unpack
  parseJSONValue :: J.Value -> J.Parser a
  parseJSONValue (J.Array v) | V.length v == 1 = J.parseJSON (V.head v)
  parseJSONValue j = J.parseJSON j
  parseStream, parseStream1 :: JS.Parser a
  parseStream = parseStream1 <|> 0 JS..! parseStream1
  renderValue :: a -> B.Builder
  renderValue = J.fromEncoding . J.toEncoding

typeValue1 :: Typed a => a -> Value
typeValue1 = typeValue . Identity

instance Typed Word64 where
  typeValue = ULong
  toDouble = fromIntegral
  parseStream1 = JS.integer
  renderValue = B.word64Dec
instance Typed Int64  where
  typeValue = Long
  toDouble = fromIntegral
  parseStream1 = JS.integer
  renderValue = B.int64Dec
instance Typed Int32  where
  typeValue = Integer
  toDouble = fromIntegral
  parseStream1 = JS.integer
  renderValue = B.int32Dec
instance Typed Int16  where
  typeValue = Short
  toDouble = fromIntegral
  parseStream1 = JS.integer
  renderValue = B.int16Dec
instance Typed Int8   where
  typeValue = Byte
  toDouble = fromIntegral
  parseStream1 = JS.integer
  renderValue = B.int8Dec
instance Typed Double where
  typeValue = Double
  toDouble = id
  parseStream1 = JS.real
  renderValue = B.doubleDec
instance Typed Float  where
  typeValue = Float
  toDouble = realToFrac
  parseStream1 = JS.real
  renderValue = B.floatDec
instance Typed Half   where
  typeValue = HalfFloat
  toDouble = realToFrac
  parseStream1 = JS.real
  renderValue = B.floatDec . realToFrac
instance Typed Bool   where
  typeValue = Boolean
  toDouble False = 0
  toDouble True = 1
  fromInt = toEnum
  readValue "0" = Just False
  readValue "1" = Just True
  readValue "false" = Just False
  readValue "true" = Just True
  readValue "False" = Just False
  readValue "True" = Just True
  readValue _ = Nothing
  parseJSONValue (J.Bool b) = return b
  parseJSONValue (J.Number 0) = return False
  parseJSONValue (J.Number 1) = return True
  parseJSONValue (J.Array v) | V.length v == 1 = parseJSONValue (V.head v)
  parseJSONValue j = J.typeMismatch "Bool" j
  parseStream1 = JS.bool <|> toEnum <$> JS.integer
  renderValue False = "false"
  renderValue True = "true"
instance Typed T.Text where
  typeValue = Keyword
  toDouble = read . T.unpack
  fromInt = T.pack . show
  readValue = Just . TE.decodeUtf8With TE.lenientDecode
  parseStream1 = JS.string
  renderValue = TE.encodeUtf8Builder
instance Typed Void   where
  typeValue = Void
  toDouble = absurd
  fromInt _ = error "fromInt Void"
  parseStream1 = empty
  renderValue = absurd
instance Typed a => Typed (V.Vector a) where
  typeValue = Array . typeValue . Compose
  toDouble = toDouble . V.head
  fromInt = V.singleton . fromInt
  parseJSONValue (J.Array v) = V.mapM parseJSONValue v
  parseJSONValue j = V.singleton <$> parseJSONValue j
  parseStream1 = V.singleton <$> parseStream1
  parseStream = V.fromList <$> many (JS.arrayOf parseStream1) <|> parseStream1

unTypeValue :: (forall a . Typed a => f a -> b) -> TypeValue f -> b
unTypeValue f (Double    x) = f $! x
unTypeValue f (Float     x) = f $! x
unTypeValue f (HalfFloat x) = f $! x
unTypeValue f (Long      x) = f $! x
unTypeValue f (ULong     x) = f $! x
unTypeValue f (Integer   x) = f $! x
unTypeValue f (Short     x) = f $! x
unTypeValue f (Byte      x) = f $! x
unTypeValue f (Boolean   x) = f $! x
unTypeValue f (Keyword   x) = f $! x
unTypeValue f (Array     x) = unTypeValue (f . getCompose) x
unTypeValue f (Void      x) = f x

traverseTypeValue :: Functor m => (forall a . Typed a => f a -> m (h a)) -> TypeValue f -> m (TypeValue h)
traverseTypeValue f (Double    x) = Double    <$> f x
traverseTypeValue f (Float     x) = Float     <$> f x
traverseTypeValue f (HalfFloat x) = HalfFloat <$> f x
traverseTypeValue f (Long      x) = Long      <$> f x
traverseTypeValue f (ULong     x) = ULong     <$> f x
traverseTypeValue f (Integer   x) = Integer   <$> f x
traverseTypeValue f (Short     x) = Short     <$> f x
traverseTypeValue f (Byte      x) = Byte      <$> f x
traverseTypeValue f (Boolean   x) = Boolean   <$> f x
traverseTypeValue f (Keyword   x) = Keyword   <$> f x
traverseTypeValue f (Array     x) = Array     <$> traverseTypeValue (fmap Compose . f . getCompose) x
traverseTypeValue f (Void      x) = Void      <$> f x

traverseTypeValue2 :: (Functor f, Functor g, MonadFail m) => (forall a . Typed a => f a -> g a -> m (h a)) -> TypeValue f -> TypeValue g -> m (TypeValue h)
traverseTypeValue2 f (Double    x) (Double    y) = Double    <$> f x y
traverseTypeValue2 f (Float     x) (Float     y) = Float     <$> f x y
traverseTypeValue2 f (HalfFloat x) (HalfFloat y) = HalfFloat <$> f x y
traverseTypeValue2 f (Long      x) (Long      y) = Long      <$> f x y
traverseTypeValue2 f (ULong     x) (ULong     y) = ULong     <$> f x y
traverseTypeValue2 f (Integer   x) (Integer   y) = Integer   <$> f x y
traverseTypeValue2 f (Short     x) (Short     y) = Short     <$> f x y
traverseTypeValue2 f (Byte      x) (Byte      y) = Byte      <$> f x y
traverseTypeValue2 f (Boolean   x) (Boolean   y) = Boolean   <$> f x y
traverseTypeValue2 f (Keyword   x) (Keyword   y) = Keyword   <$> f x y
traverseTypeValue2 f (Array     x) (Array     y) = Array     <$> traverseTypeValue2 (\x' y' -> Compose <$> f (getCompose x') (getCompose y')) x y
traverseTypeValue2 f (Void      x) v             = traverseTypeValue (f (vacuous x)) v
traverseTypeValue2 f v             (Void      y) = traverseTypeValue (\x -> f x (vacuous y)) v
traverseTypeValue2 _ _             _             = fail "traverseTypeValue2: type mismatch"

-- isn't there a Functor1 class for this or something?
fmapTypeValue :: (forall a . Typed a => f a -> g a) -> TypeValue f -> TypeValue g
fmapTypeValue f = runIdentity . traverseTypeValue (Identity . f)

fmapTypeValue1 :: (forall a . Typed a => f a -> a) -> TypeValue f -> Value
fmapTypeValue1 f = fmapTypeValue (Identity . f)

fmapTypeValue2 :: (Functor f, Functor g) => (forall a . Typed a => f a -> g a -> h a) -> TypeValue f -> TypeValue g -> TypeValue h
fmapTypeValue2 f = (fromMaybe (error "fmapTypeValue2: type mismatch") .) . traverseTypeValue2 ((Just .) . f)

coerceTypeValue :: (Functor f, Functor g) => TypeValue f -> TypeValue g -> TypeValue g
coerceTypeValue = fmapTypeValue2 (\_ -> id)

makeTypeValue :: Type -> (forall a . Typed a => f a) -> TypeValue f
makeTypeValue f t = fmapTypeValue (\Proxy -> t) f

makeTypeValueM :: Functor m => Type -> (forall a . Typed a => m (f a)) -> m (TypeValue f)
makeTypeValueM f t = traverseTypeValue (\Proxy -> t) f

class Traversable f => TypeTraversable f where
  sequenceTypeValue :: f Value -> TypeValue f

instance TypeTraversable [] where
  sequenceTypeValue [] = Void [] -- hrm, usually fixed by coerce
  sequenceTypeValue [v] = fmapTypeValue ((:[]) . runIdentity) v
  sequenceTypeValue (v:l) = fmapTypeValue2 ((:) . runIdentity) v (sequenceTypeValue l)

instance TypeTraversable Maybe where
  sequenceTypeValue Nothing = Void Nothing -- hrm, usually fixed by coerce
  sequenceTypeValue (Just v) = fmapTypeValue (Just . runIdentity) v

traverseValue :: Functor g => (forall a . Typed a => f a -> g a) -> TypeValue f -> g Value
traverseValue f = traverseTypeValue (fmap Identity . f)

sequenceValue :: Functor f => TypeValue f -> f Value
sequenceValue = traverseTypeValue (fmap Identity)

fmapValue :: (forall a . Typed a => a -> a) -> Value -> Value
fmapValue f = fmapTypeValue1 (f . runIdentity)

typeOfValue :: TypeValue f -> Type
typeOfValue = fmapTypeValue (const Proxy)

instance (Functor f, Eq1 f) => Eq (TypeValue f) where
  a == b = maybe False (unTypeValue getConst) $ traverseTypeValue2 (\x y -> Just $ Const $ eq1 x y) a b

instance {-# OVERLAPPABLE #-} Show1 f => Show (TypeValue f) where
  showsPrec i = unTypeValue (showsPrec1 i)

instance Show Value where
  showsPrec i = unTypeValue (showsPrec i . runIdentity)

instance {-# OVERLAPPABLE #-} J.ToJSON1 f => J.ToJSON (TypeValue f) where
  toJSON = unTypeValue J.toJSON1
  toEncoding = unTypeValue J.toEncoding1

parseJSONOrStringValue :: Typed a => J.Value -> J.Parser a
parseJSONOrStringValue j@(J.String s) = parseJSONValue j <|> maybe (fail "parseStringValue") return (readValue $ TE.encodeUtf8 s)
parseJSONOrStringValue j = parseJSONValue j

arrayHead :: Functor f => TypeValue (Compose f V.Vector) -> TypeValue f
arrayHead = fmapTypeValue (fmap V.head . getCompose)

singletonArray :: Functor f => TypeValue f -> TypeValue (Compose f V.Vector)
singletonArray = fmapTypeValue (Compose . fmap V.singleton)

typeIsArray :: Functor f => TypeValue f -> Maybe (TypeValue f)
typeIsArray (Array t) = Just $ arrayHead t
typeIsArray _ = Nothing

unArrayType :: Functor f => TypeValue f -> (Bool, TypeValue f)
unArrayType t = maybe (False, t) (True ,) $ typeIsArray t

instance Default Type where
  def = Float Proxy

instance {-# OVERLAPPING #-} Show Type where
  show (Keyword _)   = "keyword"
  show (Long _)      = "long"
  show (ULong _)     = "unsigned_long"
  show (Integer _)   = "integer"
  show (Short _)     = "short"
  show (Byte _)      = "byte"
  show (Double _)    = "double"
  show (Float _)     = "float"
  show (HalfFloat _) = "half_float"
  show (Boolean _)   = "boolean"
  show (Void _)      = "void"
  show (Array x)     = "array " <> show (arrayHead x)

instance Read Type where
  readPrec = do
    Ident s <- lexP
    case s of
      "array"       -> Array . singletonArray <$> readPrec
      "keyword"     -> return (Keyword Proxy)
      "long"        -> return (Long Proxy)
      "int64"       -> return (Long Proxy)
      "unsigned_long" -> return (ULong Proxy)
      "ulong"       -> return (ULong Proxy)
      "uint64"      -> return (ULong Proxy)
      "int"         -> return (Integer Proxy)
      "integer"     -> return (Integer Proxy)
      "int32"       -> return (Integer Proxy)
      "short"       -> return (Short Proxy)
      "int16"       -> return (Short Proxy)
      "byte"        -> return (Byte Proxy)
      "int8"        -> return (Byte Proxy)
      "double"      -> return (Double Proxy)
      "float8"      -> return (Double Proxy)
      "float64"     -> return (Double Proxy)
      "float"       -> return (Float Proxy)
      "float4"      -> return (Float Proxy)
      "float32"     -> return (Float Proxy)
      "half_float"  -> return (HalfFloat Proxy)
      "float2"      -> return (HalfFloat Proxy)
      "float16"     -> return (HalfFloat Proxy)
      "bool"        -> return (Boolean Proxy)
      "boolean"     -> return (Boolean Proxy)
      "void"        -> return (Void Proxy)
      _ -> fail "Unknown type"
      
instance {-# OVERLAPPING #-} J.ToJSON Type where
  toJSON = J.toJSON . show
  toEncoding = J.toEncoding . show

instance J.FromJSON Type where
  parseJSON = J.withText "type" $ either fail return . readEither . T.unpack

typeIsFloating :: TypeValue f -> Bool
typeIsFloating (Double    _) = True
typeIsFloating (Float     _) = True
typeIsFloating (HalfFloat _) = True
typeIsFloating _ = False

typeIsIntegral :: TypeValue f -> Bool
typeIsIntegral (Long      _) = True
typeIsIntegral (ULong     _) = True
typeIsIntegral (Integer   _) = True
typeIsIntegral (Short     _) = True
typeIsIntegral (Byte      _) = True
typeIsIntegral _ = False

typeIsNumeric :: TypeValue f -> Bool
typeIsNumeric t = typeIsFloating t || typeIsIntegral t

typeIsString :: TypeValue f -> Bool
typeIsString (Keyword   _) = True
typeIsString _ = False

typeIsBoolean :: TypeValue f -> Bool
typeIsBoolean (Boolean _) = True
typeIsBoolean _ = False

baseType :: Functor f => (a,a,a,a,a) -> TypeValue f -> a
baseType (f,i,b,s,_) t
  | typeIsFloating t' = f
  | typeIsIntegral t' = i
  | typeIsString   t' = s
  | typeIsBoolean  t' = b
  where (_,t') = unArrayType t
baseType (_,_,_,_,v) ~(Void _) = v

numpyTypeSize :: Functor f => TypeValue f -> Word
numpyTypeSize (Double    _) = 8
numpyTypeSize (Float     _) = 4
numpyTypeSize (HalfFloat _) = 2
numpyTypeSize (Long      _) = 8
numpyTypeSize (ULong     _) = 8
numpyTypeSize (Integer   _) = 4
numpyTypeSize (Short     _) = 2
numpyTypeSize (Byte      _) = 1
numpyTypeSize (Boolean   _) = 1
numpyTypeSize (Keyword   _) = 8 -- XXX overridden in numpyFieldSize
numpyTypeSize (Void      _) = 0
numpyTypeSize (Array     t) = numpyTypeSize (arrayHead t)
