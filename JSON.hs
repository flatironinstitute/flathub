module JSON
  ( parseJSONField
  , (.=*)
  , EmptyJSON(..)
  , mergeJSON, mergeJSONObject
  ) where

import qualified Data.Aeson.Types as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

parseJSONField :: T.Text -> (J.Value -> J.Parser a) -> J.Object -> J.Parser a
parseJSONField f p o = J.explicitParseField p o f

infixr 8 .=*
(.=*) :: T.Text -> J.Series -> J.Series
(.=*) f = JE.pair f . JE.pairs

data EmptyJSON = EmptyJSON

instance J.ToJSON EmptyJSON where
  toJSON EmptyJSON = J.emptyObject
  toEncoding EmptyJSON = JE.emptyObject_

instance J.FromJSON EmptyJSON where
  parseJSON J.Null = return EmptyJSON
  parseJSON (J.Object o) | HM.null o = return EmptyJSON
  parseJSON (J.Array o) | V.null o = return EmptyJSON
  parseJSON v = J.typeMismatch "EmptyJSON" v

mergeJSON :: J.Value -> J.Value -> J.Value
mergeJSON J.Null x = x
mergeJSON (J.Object a) (J.Object b) = J.Object (mergeJSONObject a b)
mergeJSON x _ = x

mergeJSONObject :: J.Object -> J.Object -> J.Object
mergeJSONObject = HM.unionWith mergeJSON
