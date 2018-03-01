{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Logger        (runNoLoggingT, runStdoutLoggingT)
import           Database.Persist.Postgresql (ConnectionPool, ConnectionString,
                                              createPostgresqlPool)

import           Lib
import           Types

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool "database" 1
  startApp $ AppConfig pool
