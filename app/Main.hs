{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Logger
import           Database.Persist.Postgresql

import           Db
import           Lib
import           Types

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool "host=localhost dbname=tododb user=dbuser password=dbpassword port=5432" 1
  runSqlPool doMigrations pool
  startApp $ AppConfig pool
