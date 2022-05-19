{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Attach
  ( attachmentPresent
  , resolveAttachment
  , attachmentResponse
  , attachmentFields
  , attachmentsBulkStream
  , zipAttachments
  , listAttachments
  , curlAttachments
  ) where

import qualified Codec.Archive.Zip.Conduit.Zip as Zip
import           Control.Monad (void, guard)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import           Data.Foldable (fold)
import           Data.Functor (($>))
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.LocalTime (utcToLocalTime, utc)
import           Data.Scientific (formatScientific, FPFormat(Fixed))
import qualified Data.Vector as V
import           Network.HTTP.Types.Header (hContentType, hContentDisposition, hCacheControl)
import           Network.HTTP.Types.Status (ok200)
import qualified Network.Wai as Wai
import           System.FilePath ((</>))
import           System.IO.Error (tryIOError)
import           System.Posix.Files (getFileStatus, isRegularFile, modificationTimeHiRes, fileSize)
import qualified Web.Route.Invertible as R
import           Web.Route.Invertible.Internal (requestRoute')
import           Web.Route.Invertible.Render (renderUrlRequestBuilder)
import           Web.Route.Invertible.Wai (waiRequest)
import           Waimwork.HTTP (quoteHTTP)

import qualified KeyedMap as KM
import Monoid
import Type
import Field
import Catalog
import Global
import Backend
import Output.Types

attachmentPresent :: TypeValue Maybe -> Bool
attachmentPresent (Boolean (Just True)) = True
attachmentPresent (Void _) = True
attachmentPresent _ = False

pathFields :: DynamicPath -> [T.Text]
pathFields = mapMaybe pathField where
  pathField (DynamicPathField f) = return f
  pathField (DynamicPathSubstitute f _ _) = return f
  pathField _ = mempty

attachmentFields :: Catalog -> Field -> [Field]
attachmentFields cat f@Field{ fieldDesc = FieldDesc{ fieldDescAttachment = Just (Attachment p n) } } =
  f : mapMaybe (`HM.lookup` catalogFieldMap cat) (pathFields p ++ pathFields n)
attachmentFields _ _ = []

pathStr :: DynamicPath -> J.Object -> String
pathStr path doc = foldMap ps path where
  ps (DynamicPathLiteral s) = s
  ps (DynamicPathField f) = rf $ HM.lookup f doc
  ps (DynamicPathSubstitute f a b) = case HM.lookup f doc of
    Just (J.String s) -> T.unpack $ T.replace a b s
    x -> rf x
  rf (Just (J.String s)) = T.unpack s
  rf (Just (J.Number n)) = formatScientific Fixed (Just 0) n -- XXX assuming integers
  rf (Just (J.Bool False)) = "0"
  rf (Just (J.Bool True)) = "1"
  rf (Just (J.Array _)) = "array"
  rf (Just (J.Object _)) = "object"
  rf (Just J.Null) = ""
  rf Nothing = "?"

resolveDynamicPath :: HM.HashMap T.Text (TypeValue Maybe) -> DynamicPath -> BSL.ByteString
resolveDynamicPath doc path = B.toLazyByteString $ foldMap ps path where
  ps (DynamicPathLiteral s) = B.string8 s
  ps (DynamicPathField f) = rf $ HM.lookup f doc
  ps (DynamicPathSubstitute f a b) = case HM.lookup f doc of
    Just (Keyword (Just s)) -> TE.encodeUtf8Builder $ T.replace a b s
    x -> rf x
  rf = maybe "?" $ unTypeValue (foldMap renderValue)

resolveAttachment :: HM.HashMap T.Text (TypeValue Maybe) -> Attachment -> (FilePath, BS.ByteString)
resolveAttachment doc Attachment{..} = (BSLC.unpack $ rp attachmentPath, BSL.toStrict $ rp attachmentName) where
  rp = resolveDynamicPath doc

attachmentResponse :: FilePath -> BS.ByteString -> M Wai.Response
attachmentResponse path name = do
  dir <- asks globalDataDir
  return $ Wai.responseFile ok200
    [ (hContentType, "application/octet-stream")
    , (hContentDisposition, "attachment; filename=" <> quoteHTTP name)
    , (hCacheControl, "public, max-age=86400")
    ] (dir </> path) Nothing

attachmentsBulkStream :: BS.ByteString -> [Field] -> IO (Word, V.Vector J.Object) -> M ((B.Builder -> IO ()) -> IO ())
attachmentsBulkStream info ats next = do
  dir <- asks globalDataDir
  let ents doc = catMaybes <$> mapM (ent doc) ats
      ent doc af@Field{ fieldDesc = FieldDesc{ fieldDescAttachment = ~(Just a) } } =
        case HM.lookup (fieldName af) doc of
          Just (J.Bool True) -> enta a doc
          Just (J.Number n) | n > 0 -> enta a doc
          _ -> return Nothing
      enta a doc = either
        (const Nothing)
        (\stat -> guard (isRegularFile stat) $>
          (Zip.ZipEntry
            { Zip.zipEntryName = Left $ T.pack $ pathStr (attachmentName a) doc
            , Zip.zipEntryTime = utcToLocalTime utc $ posixSecondsToUTCTime $ modificationTimeHiRes stat
            , Zip.zipEntrySize = Just $ fromIntegral $ fileSize stat
            , Zip.zipEntryExternalAttributes = Nothing
            }
          , Zip.zipFileData path))
        <$> tryIOError (getFileStatus path)
        where path = dir </> pathStr (attachmentPath a) doc
  return $ \chunk -> C.runConduitRes
    $ C.repeatWhileM (snd <$> liftIO next) (not . V.null)
    C..| C.concatMapM (liftIO . fmap fold . V.mapM ents)
    C..| void (Zip.zipStream Zip.ZipOptions
        { Zip.zipOpt64 = False
        , Zip.zipOptCompressLevel = 1
        , Zip.zipOptInfo = Zip.ZipInfo info
        })
    C..| C.mapM_ (liftIO . chunk . B.byteString)

attachmentsFilter :: DataArgs V.Vector -> DataArgs V.Vector
attachmentsFilter args = case V.toList (dataFields args) of
  [f@Field{ fieldType = Boolean _ }] -> args
    { dataFilters = (dataFilters args)
      { filterFields = KM.insertWith (const id) f{ fieldType = Boolean (FieldEQ [True]) }
        $ filterFields $ dataFilters args
      }
    }
  _ -> args -- TODO could be more efficient with OR filters

zipGenerator :: Wai.Request -> Catalog -> DataArgs V.Vector -> M OutputStream
zipGenerator req cat args = do
  dir <- asks globalDataDir
  let ents doc = V.mapMaybeM (ent doc) ats
      ent doc af@Field{ fieldDesc = FieldDesc{ fieldDescAttachment = ~(Just a) } }
        | any attachmentPresent (HM.lookup (fieldName af) doc) =
          enta $ resolveAttachment doc a
        | otherwise = return Nothing
      enta (path, name) = either
        (const Nothing)
        (\stat -> guard (isRegularFile stat) $>
          (Zip.ZipEntry
            { Zip.zipEntryName = Right name
            , Zip.zipEntryTime = utcToLocalTime utc $ posixSecondsToUTCTime $ modificationTimeHiRes stat
            , Zip.zipEntrySize = Just $ fromIntegral $ fileSize stat
            , Zip.zipEntryExternalAttributes = Nothing
            }
          , Zip.zipFileData dirpath))
        <$> tryIOError (getFileStatus dirpath)
        where dirpath = dir </> path
  g <- ask
  return $ OutputStream Nothing $ \chunk -> C.runConduitRes
    $ C.yield ()
    C..| void (C.mapAccumWhileM (const $ next g) (Just args'))
    C..| C.concatMapM (liftIO . fmap fold . V.mapM ents)
    C..| void (Zip.zipStream Zip.ZipOptions
        { Zip.zipOpt64 = False
        , Zip.zipOptCompressLevel = 1
        , Zip.zipOptInfo = Zip.ZipInfo $ TE.encodeUtf8 (catalogTitle cat <> (foldMap (T.cons ' ' . fieldName) ats)) <> " downloaded from " <> Wai.rawPathInfo req
        })
    C..| C.mapM_ (lift . chunk . B.byteString)
  where
  ats = dataFields args
  args' = (attachmentsFilter args)
    { dataFields = KM.fromList $ foldMap (attachmentFields cat) (dataFields args)
    }
  next g (Just a) = do
    (r, o) <- lift $ runGlobal g $ queryData cat a
    return $ Right (setDataOffset a <$> o, r)
  next _ s = return $ Left s

zipAttachments :: OutputFormat
zipAttachments = OutputFormat
  { outputMimeType = "application/zip"
  , outputExtension = "zip"
  , outputDescription = "A ZIP file containing all the selected attachments"
  , outputGenerator = zipGenerator
  }

type AttachmentApi = R.Route (Simulation, T.Text, T.Text)

attachmentsStreamUrls :: AttachmentApi -> B.Builder -> (B.Builder -> B.Builder) -> B.Builder
  -> Wai.Request -> Catalog -> DataArgs V.Vector -> M OutputStream
attachmentsStreamUrls api hd row ft req cat args =
  outputStreamRows
    Nothing
    hd
    (\r -> let Just (Keyword (Just i), l) = V.uncons r in fold $ V.zipWith
      (\f v -> mwhen (attachmentPresent v) $
        row $ renderUrlRequestBuilder
          (requestRoute' api (catalogName cat, fieldName f, i) (waiRequest req) { R.requestQuery = mempty }) mempty)
      ats l)
    ft
    cat args'
  where
  ats = dataFields args
  args' = (attachmentsFilter args)
    { dataFields = V.cons idField ats
    }

listGenerator :: AttachmentApi -> Wai.Request -> Catalog -> DataArgs V.Vector -> M OutputStream
listGenerator api req cat args = attachmentsStreamUrls api
  ("# " <> TE.encodeUtf8Builder (catalogTitle cat) <> " attachments " <> mintersperseMap "," (TE.encodeUtf8Builder . fieldName) (V.toList $ dataFields args) <> "\n")
  (<> "\n")
  mempty
  req cat args

listAttachments :: AttachmentApi -> OutputFormat
listAttachments api = OutputFormat
  { outputMimeType = "text/uri-list"
  , outputExtension = "uris"
  , outputDescription = "A text file listing all the URLs of the selected attachments, one per line, with a one line #comment header"
  , outputGenerator = listGenerator api
  }

curlGenerator :: AttachmentApi -> Wai.Request -> Catalog -> DataArgs V.Vector -> M OutputStream
curlGenerator api req cat args = attachmentsStreamUrls api
  ("#!/bin/sh\n# " <> TE.encodeUtf8Builder (catalogTitle cat) <> " attachments " <> mintersperseMap "," (TE.encodeUtf8Builder . fieldName) (V.toList $ dataFields args) <> "\n")
  (("curl -JO " <>) . (<> "\n"))
  mempty
  req cat args

curlAttachments :: AttachmentApi -> OutputFormat
curlAttachments api = OutputFormat
  { outputMimeType = "text/x-shellscript"
  , outputExtension = "sh"
  , outputDescription = "A text shell script that downloads all the selected attachments with curl -JO, one per line"
  , outputGenerator = curlGenerator api
  }
