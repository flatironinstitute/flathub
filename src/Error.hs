{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Error
  where

import           Control.Monad.Except (MonadError, throwError, ExceptT, Except, runExcept)
import           Network.HTTP.Types.Status (Status, badRequest400, notFound404, internalServerError500)

type Error = (Status, String)
type MonadErr m = MonadError Error m

type ErrT = ExceptT Error
type Err = Except Error

raise :: MonadErr m => Status -> String -> m a
raise = curry throwError

raise400, raise404, raise500 :: MonadErr m => String -> m a
raise400 = raise badRequest400
raise404 = raise notFound404
raise500 = raise internalServerError500

failErr :: MonadFail m => Err a -> m a
failErr = either (fail . snd) return . runExcept

errorErr :: Err a -> a
errorErr = either (error . snd) id . runExcept
