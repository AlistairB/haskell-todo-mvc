{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Int                 (Int64)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant                  ((:<|>), (:>), Capture, Get, JSON,
                                           Post, Proxy (Proxy), ReqBody, Server,
                                           serve)

-- data Todo = Todo
--   { id :: Int
--   , message :: String
--   } deriving Show

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

-- type Api = "todos" :> Capture "id" Int :> Get '[JSON] Todo
--       :<|> "todos" :> Capture "name" String :> Get '[JSON] Todo
--       :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] Int64


-- todos :: [Todo]
-- todos =
--   [ Todo 1 "Write haskell api"
--   , Todo 2 "Write purescript frontend"
--   ]

-- getTodo :: Int -> Server Todo
-- getTodo id = pure $ Todo 1 "omg"

