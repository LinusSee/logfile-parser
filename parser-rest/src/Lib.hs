{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Servant

import CustomParsers
import FileDb as FileDb


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

type API =
  "api" :>
        (    "users" :> Get '[JSON] [User]
        :<|> "sample" :> Get '[JSON] DummyData
        :<|> "simple-parser" :> Get '[JSON] ElementaryParser
        :<|> "parsers" :> "building-blocks" :>
             (    "complex" :> Get '[JSON] [ElementaryParser]
             :<|> "complex" :> ReqBody '[JSON] ElementaryParser :> Post '[JSON] NoContent
             )
        )

myElementaryParser :: ElementaryParser
myElementaryParser = OneOf ["Hello", "World", "!"]

startApp :: IO ()
startApp = do
  let port = 8080
  run port $ app

app :: Application
app = logStdoutDev
    $ cors (const $ Just policy)
    $ provideOptions api
    $ serve api server
  where
    policy = simpleCorsResourcePolicy
              { corsRequestHeaders = [ "content-type"]}

api :: Proxy API
api = Proxy


server :: Server API
server =
       return users
  :<|> return dummyData
  :<|> return myElementaryParser
  :<|> do
          parsers <- liftIO FileDb.readAll
          return parsers
  :<|> (\_parser -> do
          liftIO $ FileDb.save _parser
          return NoContent
        )


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

dummyData :: DummyData
dummyData = DummyData 5 "MyString1" "OtherString"
