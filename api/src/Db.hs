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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Db where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Text
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
  addItem  :: Todo -> m Int64

instance MonadTodoDb App where
  getItems = (fmap . fmap) entityVal (runDb $ selectList [] [])

  getItem todoId' = runDb $ get (toSqlKey todoId')

  addItem todoItem' = fromSqlKey <$> runDb (insert todoItem')

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader AppConfig m, MonadIO m, MonadError AppError m, MonadLogger m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks dbPool
    eitherResult <- liftIO $ (fmap . first) DbError $ try $ runSqlPool query pool
    case eitherResult of
      -- todo use logError with TH?
      Left e  -> logErrorNS "runDb" (pack $ show e) >> throwError e
      Right b -> pure b
