{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
module Lib
    ( startApp
    , app
    ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Logger     (LoggingT, MonadLogger,
                                           runStdoutLoggingT)
import           Control.Monad.Reader     (MonadReader, ReaderT, runReaderT)
import           Data.Aeson.TH            (defaultOptions, deriveJSON)
import           Data.Maybe               (listToMaybe)
import           Db
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.Server           (hoistServer)
import           Types

appToHandler :: AppConfig -> App a -> Handler a
appToHandler config = liftIO . flip runReaderT config . runStdoutLoggingT . unApp

server :: AppConfig -> Server TodoAPI
server config = hoistServer api (appToHandler config) backend

startApp :: AppConfig -> IO ()
startApp config = run 8080 (app config)

app :: AppConfig -> Application
app config = serve api (server config)

api :: Proxy TodoAPI
api = Proxy

backend :: ServerT TodoAPI App
backend = getTodos :<|> getTodo :<|> addTodo

getTodos :: App [Todo]
getTodos = pure []

getTodo :: Int -> App (Maybe Todo)
getTodo _ = pure Nothing

addTodo :: Todo -> App Int
addTodo _ = pure 1

type TodoAPI = "todos" :> Get '[JSON] [Todo]
          :<|> "todos" :> Capture "id" Int :> Get '[JSON] (Maybe Todo)
          :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] Int
