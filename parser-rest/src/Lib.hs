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
import Network.Wai.Middleware.Cors (simpleCors)
import Servant

import CustomParsers


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
      :<|> "parsers" :> "building-blocks" :> "complex" :> ReqBody '[JSON] ElementaryParser :> Post '[JSON] NoContent
      :<|> "sample" :> Get '[JSON] DummyData
      :<|> "simple-parser" :> Get '[JSON] ElementaryParser
      )

myElementaryParser :: ElementaryParser
myElementaryParser = OneOf ["Hello", "World", "!"]

startApp :: IO ()
startApp = run 8080 $ simpleCors app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
       return users
  :<|> (\_parser -> return NoContent)
  :<|> return dummyData
  :<|> return myElementaryParser--"A simple string!"


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

dummyData :: DummyData
dummyData = DummyData 5 "MyString1" "OtherString"
