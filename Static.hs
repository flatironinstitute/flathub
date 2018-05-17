{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Static
  ( static
  , staticURI
  ) where

import           Control.Monad (guard)
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import           Data.String (IsString)
import qualified Data.Text as T
import           Network.HTTP.Types.Header (hContentType, hCacheControl)
import           Network.HTTP.Types.Status (ok200)
import qualified Network.Mime as Mime
import qualified Network.Wai as Wai
import qualified System.FilePath as FP
import qualified Text.Blaze.Html5 as H hiding (text, textValue)
import qualified Waimwork.Blaze as WH
import qualified Web.Route.Invertible as R

import Global

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

static :: Route [FilePathComponent]
static = getPath ("js" R.*< R.manyI R.parameter) $ \paths _ -> do
  let path = FP.joinPath ("js" : map componentFilePath paths)
  return $ Wai.responseFile ok200
    [ (hContentType, getMimeType (T.pack path))
    , (hCacheControl, "public, max-age=" <> (if length paths == 1 then "10, must-revalidate" else "1000000"))
    ] path Nothing

staticURI :: [FilePathComponent] -> H.AttributeValue
staticURI p = WH.routeActionValue static p mempty

