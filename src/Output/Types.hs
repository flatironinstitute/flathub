{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Output.Types
  where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT, ask)
import qualified Data.ByteString.Builder as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import           Data.Foldable (toList)
import           Data.Functor.Identity (Identity(..))
import           Data.Proxy (Proxy(Proxy))
import qualified Network.HTTP.Media.MediaType as MT
import qualified Network.Wai as Wai
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified KeyedMap as KM
import Type
import Field
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

outputStreamRows' :: forall f . Traversable f => Maybe Word -> B.Builder -> (f (TypeValue Maybe) -> B.Builder) -> B.Builder
  -> Catalog -> DataArgs f -> M OutputStream
outputStreamRows' sz hd row ft cat args = do
  g <- ask
  return $ OutputStream sz $ \chunk -> do
    chunk hd
    runReaderT (C.runConduitRes
      $ queryDataStream' cat args{ dataFields = KM.fromList $ toList $ dataFields args }
      C..| C.mapM_ (liftIO . chunk . row . proc))
      g
    chunk ft
  where
  proc :: [FieldValue] -> f (TypeValue Maybe)
  proc l = fmap (\f -> fv f $ KM.lookup f m) (dataFields args)
    where
    m :: KM.KeyedMap FieldValue
    m = KM.fromList l
  fv f Nothing = fmapTypeValue (\Proxy -> Nothing) $ fieldType f
  fv _ (Just f) = fmapTypeValue (\(Identity x) -> Just x) $ fieldType f

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
