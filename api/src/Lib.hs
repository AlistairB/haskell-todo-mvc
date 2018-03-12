{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
module Lib
    ( startApp
    , app
    ) where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8
import           Data.Int
import           Database.Persist
import           Db
import           Network.Wai                (Application)
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Types

type TodoAPI = "todos" :> Get '[JSON] [Entity Todo]
          :<|> "todos" :> Capture "id" Int64 :> Get '[JSON] (Maybe (Entity Todo))
          :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] Int64

appToHandler :: AppConfig -> App a -> Handler a
appToHandler config = Handler . withExceptT toServantError . flip runReaderT config . runStdoutLoggingT . unApp

toServantError :: AppError -> ServantErr
toServantError (DbError a) = err500 { errBody = pack $ show a }

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

getTodos :: App [Entity Todo]
getTodos = getItems

getTodo :: Int64 -> App (Maybe (Entity Todo))
getTodo = getItem

addTodo :: Todo -> App Int64
addTodo  = addItem
