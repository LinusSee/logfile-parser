{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , app
    , server
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
  ( ElementaryParser
  , LogfileParser (..)
  , ParsingRequest (..)
  , ParsingResponse(ParsingError)
  , LogfileParsingRequest (..)
  , LogfileParsingResponse(LogfileParsingError)
  )
import ElementaryParserFileDb as ElemFileDb
import LogfileParserFileDb as LogFileDb
import LogfileParsing as LogfileParsing


startApp :: IO ()
startApp = do
  let port = 8080
  run port $ app

type API =
  "api" :>
      ( "parsers" :>
        (
            ( "logfile" :>
                (
                     Get '[JSON] [String]
                :<|> ReqBody '[JSON] LogfileParser :> Post '[JSON] NoContent
                :<|> "apply1" :> Capture "parserName" String :> QueryParam "target" String :> Get '[JSON] LogfileParsingResponse
                :<|> "apply" :> ReqBody '[JSON] LogfileParsingRequest :> Post '[JSON] LogfileParsingResponse
                )
            )
            :<|>
            ( "building-blocks" :>
                (    "complex" :> Get '[JSON] [ElementaryParser]
                :<|> "complex" :> ReqBody '[JSON] ElementaryParser :> Post '[JSON] NoContent
                :<|> "complex" :> "apply" :> ReqBody '[JSON] ParsingRequest :> Post '[JSON] ParsingResponse
                )
            )
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
  (    getLogfileParserNames
  :<|> saveLogfileParserHandler
  :<|> applyLogfileParserByName
  :<|> logfileParserApplicationHandler
  )
  :<|> readAllElementaryParsersHandler
  :<|> saveParserHandler
  :<|> parserApplicationHandler


getLogfileParserNames :: Handler [String]
getLogfileParserNames = do
  parsers <- liftIO LogFileDb.readAll
  return $ map extractName parsers
  where extractName ( LogfileParser name _ ) = name


saveLogfileParserHandler :: LogfileParser -> Handler NoContent
saveLogfileParserHandler logfileParser = do
  _ <- liftIO $ LogFileDb.save logfileParser
  return NoContent


applyLogfileParserByName :: String -> Maybe String -> Handler LogfileParsingResponse
applyLogfileParserByName parserName maybeTarget =
  case maybeTarget of
    Just target -> do
      parsers <- liftIO LogFileDb.readAll
      let parser = head $ filter byName parsers
      let parsingResult = LogfileParsing.applyLogfileParser target parser
      case parsingResult of
        Left err ->
          return $ LogfileParsingError (show err)
        Right result ->
          return result
    Nothing ->
      return $ LogfileParsingError "Missing query parameter 'target'"
  where byName (LogfileParser name _ ) = name == parserName


logfileParserApplicationHandler :: LogfileParsingRequest -> Handler LogfileParsingResponse
logfileParserApplicationHandler (LogfileParsingRequest target parser) = do
  let parsingResult = LogfileParsing.applyLogfileParser target parser
  case parsingResult of
    Left err ->
      return $ LogfileParsingError (show err)
    Right result ->
      return result


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
