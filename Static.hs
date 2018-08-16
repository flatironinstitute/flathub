{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Static
  ( FilePathComponent(..)
  , getModificationTime'
  , static
  , staticURI
  ) where

import           Control.Exception (handleJust)
import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import           Data.String (IsString)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Network.HTTP.Types.Header (hContentType, hCacheControl)
import           Network.HTTP.Types.Status (ok200)
import qualified Network.Mime as Mime
import qualified Network.Wai as Wai
import           System.Directory (getModificationTime)
import qualified System.FilePath as FP
import           System.IO.Error (isDoesNotExistError)
import qualified Text.Blaze.Html5 as H hiding (text, textValue)
import qualified Waimwork.Blaze as WH
import qualified Web.Route.Invertible as R

import Global
import Compression

newtype FilePathComponent = FilePathComponent{ componentFilePath :: String }
  deriving (IsString)

instance R.Parameter R.PathString FilePathComponent where
  parseParameter p = do
    s <- R.parseParameter p
    guard $ FP.isValid s && head s /= '.' && not (any FP.isPathSeparator s)
    return $ FilePathComponent s
  renderParameter (FilePathComponent s) = R.renderParameter s

getMimeType :: Mime.FileName -> Mime.MimeType
getMimeType = Mime.mimeByExt (Map.insert "ts" "text/typescript" Mime.defaultMimeMap) Mime.defaultMimeType

getModificationTime' :: FilePath -> IO (Maybe UTCTime)
getModificationTime' f =
  handleJust (guard . isDoesNotExistError) (\() -> return Nothing) $ Just <$> getModificationTime f

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (a:l) = do
  r <- f a
  if r then return $ Just a else findM f l

static :: Route [FilePathComponent]
static = getPath ("web" R.*< R.manyI R.parameter) $ \paths q -> liftIO $ do
  let path = FP.joinPath ("web" : map componentFilePath paths)
      encs = acceptCompressionEncoding q
  fmod <- getModificationTime' path
  enc <- findM (\e -> do
    zmod <- getModificationTime' (compressionFilename (Just e) path)
    return $ zmod >= fmod) encs
  return $ Wai.responseFile ok200 (
    [ (hContentType, getMimeType (T.pack path))
    , (hCacheControl, "public, max-age=" <> (if length paths == 1 then "10, must-revalidate" else "1000000"))
    ] ++ compressionEncodingHeader enc) (compressionFilename enc path) Nothing

staticURI :: [FilePathComponent] -> H.AttributeValue
staticURI p = WH.routeActionValue static p mempty

