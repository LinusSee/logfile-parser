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
import Data.Aeson as Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Servant
import Data.ByteString.Char8 (pack)
import Data.CaseInsensitive (mk)

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
import qualified Configs as Configs
import qualified ParsingOrchestration as Orchestration
import qualified ValidationOrchestration as ValidationOrchestration


startApp :: Configs.Config -> IO ()
startApp (Configs.Config fileDbConfig apiConfig) = do
  let port = Configs.port apiConfig
  run port $ app fileDbConfig

type API =
  "api" :>
      ( "parsers" :>
        (
            ( "logfile" :>
                (
                     Get '[JSON] [String]
                :<|> ReqBody '[JSON] CreateLogfileParserRequest :> PostCreated '[JSON] NoContent
                :<|> "apply" :> Capture "parserName" String :> QueryParam "target" String :> Get '[JSON] LogfileParsingResponse
                :<|> "apply" :> ReqBody '[JSON] LogfileParsingRequest :> Post '[JSON] LogfileParsingResponse
                )
            )
            :<|>
            ( "building-blocks" :> "complex" :>
                (    Get '[JSON] [ElementaryParser]
                :<|> ReqBody '[JSON] ElementaryParser :> PostCreated '[JSON] NoContent
                :<|> "apply" :> Capture "parserName" String :> QueryParam "target" String :> Get '[JSON] ParsingResponse
                :<|> "apply" :> ReqBody '[JSON] ParsingRequest :> Post '[JSON] ParsingResponse
                )
            )
        )
      )


app :: Configs.FileDbConfig -> Application
app dbConfig = logStdoutDev
    $ cors (const $ Just policy)
    $ provideOptions api
    $ serve api (server dbConfig)
  where
    policy = simpleCorsResourcePolicy
              { corsRequestHeaders = [ "content-type"]}


api :: Proxy API
api = Proxy


server :: Configs.FileDbConfig -> Server API
server dbConfig =
  (    getLogfileParserNames dbConfig
  :<|> saveLogfileParserHandler dbConfig
  :<|> applyLogfileParserByName dbConfig
  :<|> logfileParserApplicationHandler
  )
  :<|> readAllElementaryParsersHandler dbConfig
  :<|> saveParserHandler dbConfig
  :<|> applyParserByNameHandler dbConfig
  :<|> applyParserHandler


getLogfileParserNames ::  Configs.FileDbConfig -> Handler [String]
getLogfileParserNames dbConfig = do
  response <- liftIO $ Orchestration.existingLogfileParserNames dbConfig

  return response


saveLogfileParserHandler ::  Configs.FileDbConfig -> CreateLogfileParserRequest -> Handler NoContent
saveLogfileParserHandler dbConfig logfileParser = do
  _ <- liftIO $ Orchestration.createLogfileParser dbConfig logfileParser

  return NoContent


applyLogfileParserByName ::  Configs.FileDbConfig -> String -> Maybe String -> Handler LogfileParsingResponse
applyLogfileParserByName dbConfig parserName maybeTarget =
  case maybeTarget of
    Just target -> do
      response <- liftIO $ Orchestration.applyLogfileParserByName dbConfig parserName target
      return response

    Nothing ->
      -- return $ LogfileParsingError "Missing query parameter 'target'"
      throwError $ err404 { errBody = "Opps, that went wrong"}


logfileParserApplicationHandler :: LogfileParsingRequest -> Handler LogfileParsingResponse
logfileParserApplicationHandler request = do
  let response = Orchestration.applyLogfileParser request

  return response


readAllElementaryParsersHandler ::  Configs.FileDbConfig -> Handler [ElementaryParser]
readAllElementaryParsersHandler dbConfig = do
  response <- liftIO $ Orchestration.existingElementaryParsers dbConfig

  return response


saveParserHandler ::  Configs.FileDbConfig -> ElementaryParser -> Handler NoContent
saveParserHandler dbConfig parser = do
  _ <- liftIO $ Orchestration.createElementaryParser dbConfig parser

  return NoContent


applyParserByNameHandler ::  Configs.FileDbConfig -> String -> Maybe String -> Handler ParsingResponse
applyParserByNameHandler dbConfig parserName maybeTarget =
  case validatedParams of
    Right (validParserName, validTarget) -> do
      response <- liftIO $ Orchestration.applyElementaryParserByName dbConfig validParserName validTarget

      return response

    Left problem ->
      throwError $ err400
        { errBody = encode problem
        , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
        }

  where validatedParams = ValidationOrchestration.validateParsingUrlRequest parserName maybeTarget



applyParserHandler :: ParsingRequest -> Handler ParsingResponse
applyParserHandler request =
  case validatedRequest of
    Right validRequest -> do
      let response = Orchestration.applyElementaryParser request

      return response

    Left problem ->
      throwError $ err400
        { errBody = encode problem
        , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
        }

  where validatedRequest = ValidationOrchestration.validateParsingRequest request
