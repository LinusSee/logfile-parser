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
  ( ElementaryParser (..)
  , LogfileParser (..)
  , ParsingRequest (..)
  , ParsingResponse (ParsingError)
  , LogfileParsingRequest (..)
  , LogfileParsingResponse (LogfileParsingError)
  )
import qualified ElementaryParserFileDb as ElemFileDb
import qualified LogfileParserFileDb as LogFileDb
import qualified LogfileParsing as LogfileParsing
import qualified ElementaryParsing as ElementaryParsing
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
                :<|> ReqBody '[JSON] LogfileParser :> Post '[JSON] NoContent
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
      response <- liftIO $ Orchestration.applyLogfileParserByName parserName target
      return response
    Nothing ->
      return $ LogfileParsingError "Missing query parameter 'target'"


logfileParserApplicationHandler :: LogfileParsingRequest -> Handler LogfileParsingResponse
logfileParserApplicationHandler request = do
  response <- liftIO $ Orchestration.applyLogfileParser request
  return response


readAllElementaryParsersHandler :: Handler [ElementaryParser]
readAllElementaryParsersHandler = do
  parsers <- liftIO ElemFileDb.readAll
  return parsers


saveParserHandler :: ElementaryParser -> Handler NoContent
saveParserHandler parser = do
  _ <- liftIO $ ElemFileDb.save parser
  return NoContent


applyParserByName :: String -> Maybe String -> Handler ParsingResponse
applyParserByName parserName maybeTarget =
  case maybeTarget of
    Just target -> do
      response <- liftIO $ Orchestration.applyElementaryParserByName parserName target
      return response

    Nothing ->
      return $ ParsingError "Missing query parameter 'target'"



parserApplicationHandler :: ParsingRequest -> Handler ParsingResponse
parserApplicationHandler request = do
  response <- liftIO $ Orchestration.applyElementaryParser request
  return response
