{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Logger        (runNoLoggingT, runStdoutLoggingT)
import           Database.Persist.Postgresql (ConnectionPool, ConnectionString,
                                              createPostgresqlPool, runSqlPool)

import           Db
import           Lib
import           Types

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool "host=database dbname=tododb user=dbuser password=dbpassword port=5432" 1
  runSqlPool doMigrations pool
  startApp $ AppConfig pool
