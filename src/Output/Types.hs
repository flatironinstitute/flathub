{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Output.Types
  where

import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString.Builder as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Network.HTTP.Media.MediaType as MT
import qualified Network.Wai as Wai
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified KeyedMap as KM
import Field
import Catalog
import Backend
import Global

data OutputStream = OutputStream
  { outputSize :: Maybe Word
  , outputStream :: forall i . C.ConduitT i B.Builder (ResourceT (ReaderT Global IO)) ()
  }

outputStreamRows :: (Foldable f, DataRow (f Field) a) =>
  Maybe Word -> B.Builder -> (a -> B.Builder) -> B.Builder
  -> Catalog -> DataArgs f -> M OutputStream
outputStreamRows sz hd row ft cat args =
  return $ OutputStream sz $ do
    C.yield hd
    queryDataStream cat args C..| C.map row
    C.yield ft

type OutputGenerator = Wai.Request -> Catalog -> DataArgs V.Vector -> M OutputStream

data OutputFormat = OutputFormat
  { outputMimeType :: MT.MediaType
  , outputExtension :: String
  , outputDescription :: T.Text
  , outputGenerator :: OutputGenerator
  }

instance KM.Keyed OutputFormat where
  type Key OutputFormat = T.Text
  key = T.pack . outputExtension
