{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Half
  ( Half
  ) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OA
import           Data.Proxy (Proxy(Proxy))
import           Numeric.Half (Half)

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

instance OA.ToParamSchema Half where
  toParamSchema _ = (OA.toParamSchema (Proxy :: Proxy Float))
    { OA._schemaTitle = Just "half_float" }

instance OA.ToSchema Half where
  declareNamedSchema = return . OA.NamedSchema Nothing . OA.paramSchemaToSchema
