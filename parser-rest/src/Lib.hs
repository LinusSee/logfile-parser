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


startApp :: IO ()
startApp = do
  let port = 8080
  run port $ app

type API =
  "api" :>
        ("parsers" :> "building-blocks" :>
             (    "complex" :> Get '[JSON] [ElementaryParser]
             :<|> "complex" :> ReqBody '[JSON] ElementaryParser :> Post '[JSON] NoContent
             )
        )

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
       do
          parsers <- liftIO FileDb.readAll
          return parsers
  :<|> (\_parser -> do
          liftIO $ FileDb.save _parser
          return NoContent
        )
