{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Db where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bifunctor
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Int
import           Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo json
    todoId Int
    name String
|]

class Monad m => MonadTodoDb m where
  getItems :: m [Todo]
  getItem  :: Int64 -> m (Maybe Todo)
  addItem  :: Todo -> m ()

instance MonadTodoDb App where
  getItems = undefined

  getItem todoId' = do
    eitherResult <- runDb $ get (toSqlKey todoId')
    case eitherResult of
      Left e -> throwError e
      Right a -> pure a

  addItem = undefined

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader AppConfig m, MonadIO m) => SqlPersistT IO b -> m (Either AppError b)
runDb query = do
    pool <- asks dbPool
    liftIO $ (fmap . first) DbError $ try $ runSqlPool query pool

-- runDb :: (MonadReader AppConfig m, MonadIO m) => SqlPersistT IO b -> m b
-- runDb query = do
--     pool <- asks dbPool
--     liftIO $ runSqlPool query pool
