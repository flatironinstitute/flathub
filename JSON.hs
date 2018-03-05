module JSON
  ( parseJSONField
  ) where

import qualified Data.Aeson.Types as J
import qualified Data.Text as T

parseJSONField :: T.Text -> (J.Value -> J.Parser a) -> J.Object -> J.Parser a
parseJSONField f p o = J.explicitParseField p o f
