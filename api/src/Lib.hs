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
import           Db
import           Network.Wai                (Application)
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Types

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

getTodos :: App [Todo]
getTodos = getTodos

getTodo :: Int -> App (Maybe Todo)
getTodo = getTodo

addTodo :: Todo -> App Int
addTodo  = addTodo

type TodoAPI = "todos" :> Get '[JSON] [Todo]
          :<|> "todos" :> Capture "id" Int :> Get '[JSON] (Maybe Todo)
          :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] Int
