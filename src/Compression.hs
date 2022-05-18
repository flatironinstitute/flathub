{-# LANGUAGE OverloadedStrings #-}

module Compression
  ( CompressionFormat(..)
  , encodingCompressions
  , compressionEncoding
  , compressionEncodingHeader
  , acceptCompressionEncoding
  , compressionExtension
  , compressionFilename
  , compressionMimeType
  , decompressExtension
  , decompressFile
  , compressStream
  ) where

import           Control.Exception (throwIO)
import           Control.Monad (unless)
import qualified Codec.Compression.BZip as BZ
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BSB (flush)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Function (fix)
import qualified Data.Streaming.ByteString.Builder as SB
import qualified Data.Streaming.Zlib as SZ
import           Data.String (IsString)
import           Network.HTTP.Types.Header (Header, hAcceptEncoding, hContentEncoding)
import qualified Network.Wai as Wai
import           System.FilePath (splitExtension, (<.>))
import           Waimwork.HTTP (splitHTTP)

data CompressionFormat
  = CompressionGZip
  | CompressionBZip2
  deriving (Show, Eq, Enum, Bounded)

encodingCompressions :: [CompressionFormat]
encodingCompressions = [CompressionGZip]

compressionEncoding :: CompressionFormat -> BS.ByteString
compressionEncoding CompressionGZip = "gzip"
compressionEncoding CompressionBZip2 = "bzip2"

compressionEncodingHeader :: CompressionFormat -> Header
compressionEncodingHeader = (,) hContentEncoding . compressionEncoding

acceptCompressionEncoding :: Wai.Request -> [CompressionFormat]
acceptCompressionEncoding req = filter (\c -> compressionEncoding c `elem` enc) encodingCompressions where
  enc = maybe [] splitHTTP $ lookup hAcceptEncoding $ Wai.requestHeaders req

compressionExtension :: CompressionFormat -> String
compressionExtension CompressionGZip = "gz"
compressionExtension CompressionBZip2 = "bz2"

compressionFilename :: Maybe CompressionFormat -> FilePath -> FilePath
compressionFilename = maybe id (flip (<.>) . compressionExtension)

compressionMimeType :: IsString s => CompressionFormat -> s
compressionMimeType CompressionGZip = "application/gzip"
compressionMimeType CompressionBZip2 = "application/x-bzip2"

decompressExtension :: FilePath -> (FilePath, Maybe CompressionFormat)
decompressExtension f = case splitExtension f of
  (b, ".gz") -> (b, Just CompressionGZip)
  (b, ".bz2") -> (b, Just CompressionBZip2)
  _ -> (f, Nothing)

decompress :: CompressionFormat -> BSLC.ByteString -> BSLC.ByteString
decompress CompressionGZip = GZ.decompress
decompress CompressionBZip2 = BZ.decompress

decompressMaybe :: Maybe CompressionFormat -> BSLC.ByteString -> BSLC.ByteString
decompressMaybe = maybe id decompress

decompressFile :: FilePath -> IO BSLC.ByteString
decompressFile f = decompressMaybe (snd $ decompressExtension f) <$> BSLC.readFile f

compressStream :: Maybe CompressionFormat -> Wai.StreamingBody -> Wai.StreamingBody
compressStream Nothing body = body
compressStream (Just CompressionGZip) body = \sendChunk flush -> do
  -- based on Network.Wai.Middleware.Gzip:
  (blazeRecv, _) <- SB.newByteStringBuilderRecv SB.defaultStrategy
  deflate <- SZ.initDeflate 3 (SZ.WindowBits 31)
  let sendBuilder builder = do
        popper <- blazeRecv builder
        fix $ \loop -> do
          bs <- popper
          unless (BS.null bs) $ do
            sendBS bs
            loop
      sendBS bs = SZ.feedDeflate deflate bs >>= deflatePopper
      flushBuilder = do
        sendBuilder BSB.flush
        deflatePopper $ SZ.flushDeflate deflate
        flush
      deflatePopper popper = fix $ \loop -> do
        result <- popper
        case result of
          SZ.PRDone -> return ()
          SZ.PRNext bs' -> do
            sendChunk $ BSB.byteString bs'
            loop
          SZ.PRError e -> throwIO e
  body sendBuilder flushBuilder
  sendBuilder BSB.flush
  deflatePopper $ SZ.finishDeflate deflate
compressStream (Just z) _ = \_ _ -> fail $ "Unsupported compression format: " ++ show z
