{-# LANGUAGE OverloadedStrings #-}

module Attach
  ( attachment
  , attachmentsFields
  , attachmentsBulkStream
  ) where

import qualified Codec.Archive.Zip.Conduit.Zip as Zip
import           Control.Monad (void, guard, (<=<))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import           Data.Foldable (fold)
import           Data.Functor (($>))
import qualified Data.HashMap.Strict as HM
import           Data.List (nub)
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.LocalTime (utcToLocalTime, utc)
import           Data.Scientific (formatScientific, FPFormat(Fixed))
import qualified Data.Vector as V
import           Network.HTTP.Types.Header (hContentType, hContentDisposition, hCacheControl)
import           Network.HTTP.Types.Status (ok200, notFound404)
import qualified Network.Wai as Wai
import           System.FilePath ((</>))
import           System.IO.Error (tryIOError)
import           System.Posix.Files (getFileStatus, isRegularFile, modificationTimeHiRes, fileSize)
import           Waimwork.HTTP (quoteHTTP)
import           Waimwork.Response (response)
import           Waimwork.Result (result)
import qualified Web.Route.Invertible as R

import JSON
import Type
import Field
import Catalog
import Global
import qualified ES

askAttachment :: Catalog -> T.Text -> M Attachment
askAttachment cat att = do
  maybe (result $ response notFound404 [] ("No such attachment" :: String)) return
    $ fieldAttachment <=< HM.lookup att $ catalogFieldMap cat

pathFields :: DynamicPath -> [T.Text]
pathFields = mapMaybe pathField where
  pathField (DynamicPathField f) = return f
  pathField (DynamicPathSubstitute f _ _) = return f
  pathField _ = mempty

attachmentFields :: Attachment -> [T.Text]
attachmentFields (Attachment p n) = pathFields p ++ pathFields n

attachmentsFields :: Catalog -> [Attachment] -> [Field]
attachmentsFields cat =
  mapMaybe (`HM.lookup` catalogFieldMap cat) . nub . concatMap attachmentFields

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

attachment :: Route (Simulation, T.Text, T.Text)
attachment = getPath (R.parameter R.>* "attachment" R.>*<< R.parameter R.>*< R.parameter) $ \(sim, atn, rid) _ -> do
  cat <- askCatalog sim
  att <- askAttachment cat atn
  res <- ES.queryIndex cat mempty
    { queryLimit = 1
    , queryFilter = [idField{ fieldSub = Proxy, fieldType = Keyword (FilterEQ rid) }]
    , queryFields = attachmentsFields cat [att]
    }
  doc <- either (result . response notFound404 [] . ("Could not get item: " <>)) return
    $ J.parseEither (parse $ catalogStoreField $ catalogStore cat) res
  dir <- asks globalDataDir
  return $ Wai.responseFile ok200
    [ (hContentType, "application/octet-stream")
    , (hContentDisposition, "attachment; filename=" <> quoteHTTP (BSC.pack $ pathStr (attachmentName att) doc))
    , (hCacheControl, "public, max-age=86400")
    ] (dir </> pathStr (attachmentPath att) doc) Nothing
  where
  parse store = J.withObject "query"
    $ parseJSONField "hits" $ J.withObject "hits"
    $ parseJSONField "hits" $ J.withArray "hits"
    $ \v -> if V.length v == 1 then J.withObject "hit" (return . ES.storedFields' store) (V.head v) else fail ("found " <> show (V.length v) <> " documents")

attachmentsBulkStream :: BS.ByteString -> [Field] -> IO (Word, V.Vector J.Object) -> M ((B.Builder -> IO ()) -> IO ())
attachmentsBulkStream info ats next = do
  dir <- asks globalDataDir
  let ents doc = catMaybes <$> mapM (ent doc) ats
      ent doc af@Field{ fieldAttachment = ~(Just a) } =
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
