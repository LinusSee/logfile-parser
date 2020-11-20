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

import CustomParsers (ElementaryParser, ParsingRequest(..), ParsingResponse(ParsingError))
import ElementaryParserFileDb as ElemFileDb
import LogfileParsing as LogfileParsing


startApp :: IO ()
startApp = do
  let port = 8080
  run port $ app

type API =
  "api" :>
        "parsers" :> "logfile" :> ReqBody '[JSON] [ElementaryParser] :> Post '[JSON] NoContent :<|>
        ("parsers" :> "building-blocks" :>
             (    "complex" :> Get '[JSON] [ElementaryParser]
             :<|> "complex" :> ReqBody '[JSON] ElementaryParser :> Post '[JSON] NoContent
             :<|> "complex" :> "apply" :> ReqBody '[JSON] ParsingRequest :> Post '[JSON] ParsingResponse
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
       saveLogfileParserHandler
  :<|> readAllElementaryParsersHandler
  :<|> saveParserHandler
  :<|> parserApplicationHandler


saveLogfileParserHandler :: [ElementaryParser] -> Handler NoContent
saveLogfileParserHandler _ = do
  return NoContent

readAllElementaryParsersHandler :: Handler [ElementaryParser]
readAllElementaryParsersHandler = do
  parsers <- liftIO ElemFileDb.readAll
  return parsers


saveParserHandler :: ElementaryParser -> Handler NoContent
saveParserHandler parser = do
  _ <- liftIO $ ElemFileDb.save parser
  return NoContent

parserApplicationHandler :: ParsingRequest -> Handler ParsingResponse
parserApplicationHandler (ParsingRequest target parser) = do
  let parsingResult = LogfileParsing.applyParser target parser
  case parsingResult of
    Left err ->
      return $ ParsingError (show err)
    Right result ->
      return result
