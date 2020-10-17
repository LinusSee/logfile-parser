{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data DummyData = DummyData
  { dummy1 :: Int
  , dummy2 :: String
  , dummy3 :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''DummyData)

type API = "api" :>
      (    "users" :> Get '[JSON] [User]
      :<|> "sample" :> Get '[JSON] DummyData
      )

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
       return users
  :<|> return dummyData


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

dummyData :: DummyData
dummyData = DummyData 5 "MyString1" "OtherString"
