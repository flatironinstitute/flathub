{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Static
  ( FilePathComponent(..)
  , getModificationTime'
  , cacheControl
  , static
  , staticURI
  , staticFront
  ) where

import           Control.Exception (handleJust)
import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
import           Data.String (IsString)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Network.HTTP.Types.Header (Header, hContentType, hCacheControl)
import           Network.HTTP.Types.Status (ok200)
import qualified Network.Mime as Mime
import qualified Network.Wai as Wai
import           System.Directory (getModificationTime, doesFileExist)
import qualified System.FilePath as FP
import           System.IO.Error (isDoesNotExistError)
import qualified Text.Blaze.Html5 as H hiding (text, textValue)
import qualified Waimwork.Blaze as WH
import qualified Web.Route.Invertible as R

import Global
import Compression

newtype FilePathComponent = FilePathComponent{ componentFilePath :: String }
  deriving (IsString, Eq, Ord)

instance R.Parameter R.PathString FilePathComponent where
  parseParameter p = do
    s <- R.parseParameter p
    guard $ FP.isValid s && head s /= '.' && not (any FP.isPathSeparator s)
    return $ FilePathComponent s
  renderParameter (FilePathComponent s) = R.renderParameter s

getMimeType :: Mime.FileName -> Mime.MimeType
getMimeType = Mime.mimeByExt (Map.insert "ts" "text/typescript" Mime.defaultMimeMap) Mime.defaultMimeType

handleDoesNotExist :: IO a -> IO a -> IO a
handleDoesNotExist e = handleJust (guard . isDoesNotExistError) $ \() -> e

getModificationTime' :: FilePath -> IO (Maybe UTCTime)
getModificationTime' f =
  handleDoesNotExist (return Nothing) $ Just <$> getModificationTime f

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (a:l) = do
  r <- f a
  if r then return $ Just a else findM f l

cacheControl :: Global -> Wai.Request -> Header
cacheControl glob q = (hCacheControl, "public, max-age=" <> (if isdev then "10, must-revalidate" else "86400")) where
  isdev = globalDevMode glob && length (Wai.pathInfo q) <= 2 -- only un-cache "top-level" web/html files

staticPath :: FilePath -> Wai.Request -> M Wai.Response
staticPath path q = do
  glob <- ask
  let encs = acceptCompressionEncoding q
  fmod <- liftIO $ getModificationTime' path
  enc <- liftIO $ findM (\e -> do
    zmod <- getModificationTime' (compressionFilename (Just e) path)
    return $ zmod >= fmod) encs
  return $ Wai.responseFile ok200 (
    [ (hContentType, getMimeType (T.pack path))
    , cacheControl glob q
    ] ++ maybeToList (compressionEncodingHeader <$> enc))
    (compressionFilename enc path) Nothing

static :: Route [FilePathComponent]
static = getPath ("web" R.*< R.manyI R.parameter) $ \paths ->
  staticPath (FP.joinPath ("web" : map componentFilePath paths))

staticURI :: [FilePathComponent] -> H.AttributeValue
staticURI p = WH.routeActionValue static p mempty

staticFront :: Route [FilePathComponent]
staticFront = getPath ("v2beta" R.*< R.manyI R.parameter) $ \paths q -> do
  let path = FP.joinPath (base : map componentFilePath paths)
  s <- liftIO $ if null paths then return False else doesFileExist path
  staticPath (if s then path else base FP.</> "index.html") q
  where
  base = "flatfront/dist"
