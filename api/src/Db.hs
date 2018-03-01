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

import           Control.Monad.Reader
import           Data.Aeson             (FromJSON, ToJSON)
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.Sql
import           Database.Persist.TH
import           Database.Persist.Types
import           GHC.Generics           (Generic)
import           Types


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo json
    todoId Int
    name String
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader AppConfig m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks dbPool
    liftIO $ runSqlPool query pool
