{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Error
  where

import           Control.Monad.Except (MonadError, throwError, ExceptT, Except, runExcept)
import           Network.HTTP.Types.Header (ResponseHeaders)
import           Network.HTTP.Types.Status (Status, badRequest400, notFound404, internalServerError500)
import qualified Network.Wai as Wai
import           Waimwork.Response (response)

data Error = Error
  { errStatus :: Status
  , errHeaders :: ResponseHeaders
  , errMsg :: String
  }

errResponse :: Error -> Wai.Response
errResponse Error{..} = response errStatus errHeaders errMsg

type MonadErr m = MonadError Error m

type ErrT = ExceptT Error
type Err = Except Error

raise :: MonadErr m => Status -> String -> m a
raise s m = throwError $ Error s [] m

raise400, raise404, raise500 :: MonadErr m => String -> m a
raise400 = raise badRequest400
raise404 = raise notFound404
raise500 = raise internalServerError500

failErr :: MonadFail m => Err a -> m a
failErr = either (fail . errMsg) return . runExcept

errorErr :: Err a -> a
errorErr = either (error . errMsg) id . runExcept
