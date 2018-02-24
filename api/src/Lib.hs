{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
    ( startApp
    , app
    ) where

import           Control.Monad.Logger     (LoggingT, runStdoutLoggingT, MonadLogger)
import           Control.Monad.Reader     (ReaderT, runReaderT, MonadReader)
import           Data.Aeson.TH            (defaultOptions, deriveJSON)
import           Data.Maybe               (listToMaybe)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Servant
import Servant.Server (hoistServer)

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

type TodoAPI = "todos" :> Get '[JSON] [Todo]

v2Server :: Server TodoAPI
v2Server = hoistServer api appToHandler v2ServerBack

-- omg = hoistServer api

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api v2Server

api :: Proxy TodoAPI
api = Proxy

-- server :: Server TodoAPI
-- server = getTodos

v2ServerBack :: ServerT TodoAPI App
v2ServerBack = v2getTodos

-- getTodos :: Handler [Todo]
-- getTodos = pure todos

v2getTodos :: App [Todo]
v2getTodos = pure todos

-- getTodo :: Int -> Handler (Maybe Todo)
-- getTodo todoId' = pure $ listToMaybe $ filter ((==) todoId' . todoId) todos

-- addTodo :: Todo -> Handler Int
-- addTodo todo = pure 1

-- :<|> "todos" :> Capture "id" Int :> Get '[JSON] (Maybe Todo)
-- :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] Int

todos :: [Todo]
todos =
  [ Todo 1 "Write haskell api"
  , Todo 2 "Write purescript frontend"
  ]

$(deriveJSON defaultOptions ''Todo)
