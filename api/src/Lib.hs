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
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.Server           (hoistServer)

data Todo = Todo
  { todoId  :: Int
  , message :: String
  } deriving Show

data AppConfig = AppConfig
  { dbHost :: String
  , dbUser :: String
  , dbPass :: String
  }

newtype App a = App { unApp :: LoggingT (ReaderT AppConfig IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

appToHandler :: AppConfig -> App a -> Handler a
appToHandler config = liftIO . flip runReaderT config . runStdoutLoggingT . unApp

theConfig :: AppConfig
theConfig = AppConfig "host" "user" "pass"

server :: Server TodoAPI
server = hoistServer api (appToHandler theConfig) backend

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy TodoAPI
api = Proxy

backend :: ServerT TodoAPI App
backend = getTodos :<|> getTodo :<|> addTodo

getTodos :: App [Todo]
getTodos = pure todos

getTodo :: Int -> App (Maybe Todo)
getTodo todoId' = pure $ listToMaybe $ filter ((==) todoId' . todoId) todos

addTodo :: Todo -> App Int
addTodo _ = pure 1

type TodoAPI = "todos" :> Get '[JSON] [Todo]
          :<|> "todos" :> Capture "id" Int :> Get '[JSON] (Maybe Todo)
          :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] Int

todos :: [Todo]
todos =
  [ Todo 1 "Write haskell api"
  , Todo 2 "Write purescript frontend"
  ]

$(deriveJSON defaultOptions ''Todo)
