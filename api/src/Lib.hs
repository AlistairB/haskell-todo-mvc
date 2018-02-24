{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import           Data.Aeson.TH            (defaultOptions, deriveJSON)
import           Data.Maybe               (listToMaybe)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant

data Todo = Todo
  { todoId  :: Int
  , message :: String
  } deriving Show

$(deriveJSON defaultOptions ''Todo)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getTodos :<|> getTodo :<|> addTodo

getTodos :: Handler [Todo]
getTodos = pure todos

getTodo :: Int -> Handler (Maybe Todo)
getTodo todoId' = pure $ listToMaybe $ filter ((==) todoId' . todoId) todos

addTodo :: Todo -> Handler Int
addTodo todo = pure 1

type API = "todos" :> Get '[JSON] [Todo]
      :<|> "todos" :> Capture "id" Int :> Get '[JSON] (Maybe Todo)
      :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] Int

todos :: [Todo]
todos =
  [ Todo 1 "Write haskell api"
  , Todo 2 "Write purescript frontend"
  ]

