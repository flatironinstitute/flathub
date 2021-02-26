{-# LANGUAGE OverloadedStrings #-}

module Attach
  ( attachment
  , attachments
  ) where

import qualified Codec.Archive.Zip.Conduit.Zip as Zip
import           Control.Monad (void, guard)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J (parseEither)
import qualified Data.ByteString.Builder as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import           Data.Functor (($>))
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (mapMaybe)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.LocalTime (utcToLocalTime, utc)
import           Data.Scientific (formatScientific, FPFormat(Fixed))
import qualified Data.Vector as V
import           Network.HTTP.Types.Header (hContentType, hContentDisposition, hCacheControl)
import           Network.HTTP.Types.Status (ok200, notFound404)
import qualified Network.Wai as Wai
import           System.FilePath ((</>), takeFileName)
import           System.IO.Error (tryIOError)
import           System.Posix.Files (getFileStatus, isRegularFile, modificationTimeHiRes, fileSize)
import           Waimwork.HTTP (quoteHTTP)
import           Waimwork.Response (response)
import           Waimwork.Result (result)
import qualified Web.Route.Invertible as R

import JSON
import Field
import Catalog
import Query
import Global
import qualified ES

askAttachment :: Catalog -> T.Text -> M DynamicPath
askAttachment cat att = do
  maybe (result $ response notFound404 [] ("No such attachment" :: String)) return
    $ HM.lookup att $ catalogAttachments cat

pathFields :: Catalog -> DynamicPath -> [Field]
pathFields cat = mapMaybe pathField where
  -- assumed to exist and be unique
  pathField (DynamicPathField f) = HM.lookup f $ catalogFieldMap cat
  pathField _ = Nothing

pathStr :: DynamicPath -> J.Object -> String
pathStr path doc = foldMap ps path where
  ps (DynamicPathLiteral s) = s
  ps (DynamicPathField f) = case HM.lookup f doc of
    Just (J.String s) -> T.unpack s
    Just (J.Number n) -> formatScientific Fixed (Just 0) n -- XXX assuming integers
    Just (J.Bool False) -> "0"
    Just (J.Bool True) -> "1"
    Just (J.Array _) -> "array"
    Just (J.Object _) -> "object"
    Just J.Null -> ""
    Nothing -> "?"

attachment :: Route (Simulation, T.Text, T.Text)
attachment = getPath (R.parameter R.>* "attachment" R.>*<< R.parameter R.>*< R.parameter) $ \(sim, att, rid) _ -> do
  cat <- askCatalog sim
  paths <- askAttachment cat att
  res <- ES.queryIndex cat mempty
    { queryLimit = 1
    , queryFilter = [idField{ fieldSub = Proxy, fieldType = Text (FilterEQ rid) }]
    , queryFields = pathFields cat paths
    }
  doc <- either (result . response notFound404 [] . ("Could not get item: " <>)) return
    $ J.parseEither (parse $ catalogStoreField $ catalogStore cat) res
  dir <- asks globalDataDir
  let path = dir </> pathStr paths doc
  return $ Wai.responseFile ok200
    [ (hContentType, "application/octet-stream")
    , (hContentDisposition, "attachment; filename=" <> quoteHTTP (TE.encodeUtf8 $ T.intercalate "_" [sim, att, T.pack (takeFileName path)]))
    , (hCacheControl, "public, max-age=86400")
    ] path Nothing
  where
  parse store = J.withObject "query"
    $ parseJSONField "hits" $ J.withObject "hits"
    $ parseJSONField "hits" $ J.withArray "hits"
    $ \v -> if V.length v == 1 then J.withObject "hit" (ES.storedFields store) (V.head v) else fail ("found " <> show (V.length v) <> " documents")

attachments :: Route (Simulation, T.Text)
attachments = getPath (R.parameter R.>* "attachment" R.>*< R.parameter) $ \(sim, att) req -> do
  cat <- askCatalog sim
  paths <- askAttachment cat att
  let query = parseQuery cat req
  nextes <- ES.queryBulk cat query{ queryFields = pathFields cat paths }
  dir <- asks globalDataDir
  let ent doc = either
        (const Nothing)
        (\stat -> guard (isRegularFile stat) $>
          (Zip.ZipEntry
            { Zip.zipEntryName = Left $ T.pack $ takeFileName path
            , Zip.zipEntryTime = utcToLocalTime utc $ posixSecondsToUTCTime $ modificationTimeHiRes stat
            , Zip.zipEntrySize = Just $ fromIntegral $ fileSize stat
            , Zip.zipEntryExternalAttributes = Nothing
            }
          , Zip.zipFileData path))
        <$> tryIOError (getFileStatus path)
        where path = dir </> pathStr paths doc
  return $ Wai.responseStream ok200
    [ (hContentType, "application/zip")
    , (hContentDisposition, "attachment; filename=" <> quoteHTTP (TE.encodeUtf8 $ sim <> "_" <> att <> ".zip"))
    , (hCacheControl, "public, max-age=86400")
    ] $ \send _ -> C.runConduitRes
    $ C.repeatWhileM (snd <$> liftIO nextes) (not . V.null)
    C..| C.concatMapM (fmap (V.mapMaybe id) . V.mapM (liftIO . ent)) -- TODO: V.mapMaybeM
    C..| void (Zip.zipStream Zip.ZipOptions
        { Zip.zipOpt64 = False
        , Zip.zipOptCompressLevel = 1
        , Zip.zipOptInfo = Zip.ZipInfo
          $ TE.encodeUtf8 (catalogTitle cat <> " " <> att) <> " downloaded from " <> Wai.rawPathInfo req
        })
    C..| C.mapM_ (liftIO . send . B.byteString)
