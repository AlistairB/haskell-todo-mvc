{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger   (LoggingT)
import           Control.Monad.Reader   (ReaderT)
import           Database.Persist.Postgresql

newtype App a = App { unApp :: LoggingT (ReaderT AppConfig IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

data AppConfig = AppConfig
  { dbPool :: ConnectionPool
  }
