{-# LANGUAGE OverloadedStrings #-}

module Attach
  ( attachment
  ) where

import           Control.Monad ((<=<))
import           Control.Monad.Reader (asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J (parseEither)
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (mapMaybe)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import           Data.Scientific (formatScientific, FPFormat(Fixed))
import qualified Data.Vector as V
import           Network.HTTP.Types.Status (notFound404)
import           Network.Wai (FilePart)
import           System.FilePath ((</>))
import           Waimwork.Response (response, okResponse)
import           Waimwork.Result (result)
import qualified Web.Route.Invertible as R

import Field
import Catalog
import Global
import JSON
import qualified ES

attachment :: Route (Simulation, T.Text, T.Text)
attachment = getPath (R.parameter R.>*<< "attachment" R.*< R.parameter R.>*< R.parameter) $ \(sim, att, rid) req -> do
  cat <- askCatalog sim
  paths <- maybe (result $ response notFound404 [] ("No such attachment" :: String)) return
    $ HM.lookup att $ catalogAttachments cat
  let pathField (AttachmentPathField f) = HM.lookup f $ catalogFieldMap cat
      pathField _ = Nothing
  res <- ES.queryIndex cat mempty
    { queryLimit = 1
    , queryFilter = [idField{ fieldSub = Proxy, fieldType = Text (FilterEQ rid) }]
    , queryFields = mapMaybe pathField paths
    }
  doc <- either fail return $ J.parseEither (parse $ catalogStoreField $ catalogStore cat) res
  let pathStr (AttachmentPathLiteral s) = return s
      pathStr (AttachmentPathField f) = case HM.lookup f doc of
        Just (J.String s) -> return $ T.unpack s
        Just (J.Number n) -> return $ formatScientific Fixed (Just 0) n -- XXX assuming integers
        Nothing -> fail "missing field value"
        _ -> fail "unexpected field value"
  dir <- asks globalDataDir
  path <- (dir </>) . fold <$> mapM pathStr paths
  return $ okResponse [] (path, Nothing :: Maybe FilePart)
  where
  parse store = J.withObject "query"
    $ parseJSONField "hits" $ J.withObject "hits"
    $ parseJSONField "hits" $ J.withArray "hits"
    $ J.withObject "hit" (ES.storedFields store) <=< V.headM
