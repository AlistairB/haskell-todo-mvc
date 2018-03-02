{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Database.Persist.Postgresql

newtype App a = App { unApp :: LoggingT (ReaderT AppConfig IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadLogger)

data AppConfig = AppConfig
  { dbPool :: ConnectionPool
  }
