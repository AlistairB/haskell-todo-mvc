{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class      ()
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Database.Persist.Postgresql
import           Servant

newtype App a = App { unApp :: LoggingT (ReaderT AppConfig (ExceptT AppError IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadLogger, MonadError AppError)

data AppConfig = AppConfig
  { dbPool :: ConnectionPool
  }

data AppError =
    DbError SomeException
  | CustomApiError ServantErr
  deriving Show
