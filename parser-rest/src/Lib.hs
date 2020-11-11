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

import CustomParsers (ElementaryParser, ParsingRequest(..))
import FileDb as FileDb
import LogfileParsing as LogfileParsing


startApp :: IO ()
startApp = do
  let port = 8080
  run port $ app

type API =
  "api" :>
        ("parsers" :> "building-blocks" :>
             (    "complex" :> Get '[JSON] [ElementaryParser]
             :<|> "complex" :> ReqBody '[JSON] ElementaryParser :> Post '[JSON] NoContent
             :<|> "complex" :> "apply" :> ReqBody '[JSON] ParsingRequest :> Post '[JSON] String
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
       readAllElementaryParsersHandler
  :<|> saveParserHandler
  :<|> parserApplicationHandler


readAllElementaryParsersHandler :: Handler [ElementaryParser]
readAllElementaryParsersHandler = do
  parsers <- liftIO FileDb.readAll
  return parsers


saveParserHandler :: ElementaryParser -> Handler NoContent
saveParserHandler parser = do
  _ <- liftIO $ FileDb.save parser
  return NoContent

parserApplicationHandler :: ParsingRequest -> Handler String
parserApplicationHandler (ParsingRequest target parser) = do
  let parsingResult = LogfileParsing.applyParser target parser
  case parsingResult of
    Left err ->
      return (show err)
    Right result ->
      return result
