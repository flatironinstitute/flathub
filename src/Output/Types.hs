{-# LANGUAGE TypeFamilies #-}

module Output.Types
  where

import           Control.Monad.Reader (ask)
import qualified Data.ByteString.Builder as B
import qualified Network.HTTP.Media.MediaType as MT
import qualified Network.Wai as Wai
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified KeyedMap as KM
import Type
import Catalog
import Backend
import Global

data OutputStream = OutputStream
  { outputSize :: Maybe Word
  , outputStream :: (B.Builder -> IO ()) -> IO ()
  }

outputStreamRows :: Traversable f => Maybe Word -> B.Builder -> (f (TypeValue Maybe) -> B.Builder) -> B.Builder
  -> Catalog -> DataArgs f -> M OutputStream
outputStreamRows sz hd row ft cat args = do
  g <- ask
  return $ OutputStream sz $ \chunk -> do
    chunk hd
    let loop arg = do
          (dat, off) <- runGlobal g $ queryData cat arg
          chunk $ foldMap row dat
          mapM_ (loop . setDataOffset arg) off
    loop args
    chunk ft

type OutputGenerator = Wai.Request -> Catalog -> DataArgs V.Vector -> M OutputStream

data OutputFormat = OutputFormat
  { outputMimeType :: MT.MediaType
  , outputExtension :: String
  , outputGenerator :: OutputGenerator
  }

instance KM.Keyed OutputFormat where
  type Key OutputFormat = T.Text
  key = T.pack . outputExtension
