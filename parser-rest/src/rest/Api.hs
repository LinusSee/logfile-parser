{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}


module Api
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
  , LogfileParser
  , CreateLogfileParserRequest
  , ParsingResult (ParsingError)
  , ParsingRequest
  , ParsingResponse (ParsingResponse)
  , LogfileParsingRequest
  , LogfileParsingResponse (LogfileParsingError)
  )
import qualified ParsingOrchestration as Orchestration


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
                :<|> ReqBody '[JSON] CreateLogfileParserRequest :> Post '[JSON] NoContent
                :<|> "apply" :> Capture "parserName" String :> QueryParam "target" String :> Get '[JSON] LogfileParsingResponse
                :<|> "apply" :> ReqBody '[JSON] LogfileParsingRequest :> Post '[JSON] LogfileParsingResponse
                )
            )
            :<|>
            ( "building-blocks" :> "complex" :>
                (    Get '[JSON] [ElementaryParser]
                :<|> ReqBody '[JSON] ElementaryParser :> Post '[JSON] NoContent
                :<|> "apply" :> Capture "parserName" String :> QueryParam "target" String :> Get '[JSON] ParsingResponse
                :<|> "apply" :> ReqBody '[JSON] ParsingRequest :> Post '[JSON] ParsingResponse
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
  :<|> applyParserByName
  :<|> parserApplicationHandler


getLogfileParserNames :: Handler [String]
getLogfileParserNames = do
  response <- liftIO $ Orchestration.existingLogfileParserNames

  return response


saveLogfileParserHandler :: CreateLogfileParserRequest -> Handler NoContent
saveLogfileParserHandler logfileParser = do
  _ <- liftIO $ Orchestration.createLogfileParser logfileParser

  return NoContent


applyLogfileParserByName :: String -> Maybe String -> Handler LogfileParsingResponse
applyLogfileParserByName parserName maybeTarget =
  case maybeTarget of
    Just target -> do
      response <- liftIO $ Orchestration.applyLogfileParserByName parserName target
      return response

    Nothing ->
      return $ LogfileParsingError "Missing query parameter 'target'"


logfileParserApplicationHandler :: LogfileParsingRequest -> Handler LogfileParsingResponse
logfileParserApplicationHandler request = do
  let response = Orchestration.applyLogfileParser request

  return response


readAllElementaryParsersHandler :: Handler [ElementaryParser]
readAllElementaryParsersHandler = do
  response <- liftIO $ Orchestration.existingElementaryParsers

  return response


saveParserHandler :: ElementaryParser -> Handler NoContent
saveParserHandler parser = do
  _ <- liftIO $ Orchestration.createElementaryParser parser

  return NoContent


applyParserByName :: String -> Maybe String -> Handler ParsingResponse
applyParserByName parserName maybeTarget =
  case maybeTarget of
    Just target -> do
      response <- liftIO $ Orchestration.applyElementaryParserByName parserName target
      return response

    Nothing ->
      return $ ParsingResponse "dummyError" (ParsingError "Missing query parameter 'target'")



parserApplicationHandler :: ParsingRequest -> Handler ParsingResponse
parserApplicationHandler request = do
  let response = Orchestration.applyElementaryParser request

  return response
