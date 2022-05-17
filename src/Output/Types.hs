module Output.Types
  where

import qualified Data.ByteString.Builder as B
import qualified Network.HTTP.Media.MediaType as MT
import qualified Network.Wai as Wai
import qualified Data.Vector as V

import Type
import Catalog
import Backend

data OutputBuilder = OutputBuilder
  { outputSize :: Maybe Word
  , outputHeader :: B.Builder
  , outputRow :: V.Vector (TypeValue Maybe) -> B.Builder
  , outputFooter :: B.Builder
  }

instance Semigroup OutputBuilder where
  a <> b = OutputBuilder
    { outputSize = (+) <$> outputSize a <*> outputSize b
    , outputHeader = outputHeader a <> outputHeader b
    , outputRow = \r -> outputRow a r <> outputRow b r
    , outputFooter = outputFooter a <> outputFooter b
    }

instance Monoid OutputBuilder where
  mempty = OutputBuilder
    { outputSize = Nothing
    , outputHeader = mempty
    , outputRow = const mempty
    , outputFooter = mempty
    }

type OutputGenerator = Wai.Request -> Catalog -> DataArgs V.Vector -> OutputBuilder

data OutputFormat = OutputFormat
  { outputMimeType :: MT.MediaType
  , outputExtension :: String
  , outputGenerator :: OutputGenerator
  }
