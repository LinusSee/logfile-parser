{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}


module Api
    ( startApp
    , app
    , API
    , api
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
import Servant.Multipart
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
  , LogfileParsingFileRequest
  , LogfileParsingResponse (LogfileParsingError)
  )
import qualified CustomParsers as CM

import qualified RestParserModels as RM
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
                :<|> "apply" :> "file" :> MultipartForm Tmp LogfileParsingFileRequest :> Post '[JSON] LogfileParsingResponse
                )
            )
            :<|>
            ( "building-blocks" :> "complex" :>
                (    Get '[JSON] [RM.ElementaryParser]
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
  :<|> applyLogfileParserHandler
  :<|> applyLogfileParserToFileHandler dbConfig
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
saveLogfileParserHandler dbConfig request =
  case validatedRequest of
    Right validRequest -> do
        _ <- liftIO $ Orchestration.createLogfileParser dbConfig validRequest

        return NoContent

    Left problem ->
        throwError $ err400
          { errBody = encode problem
          , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
          }

  where validatedRequest = ValidationOrchestration.validateCreateLogfileParserRequest request


applyLogfileParserByName ::  Configs.FileDbConfig -> String -> Maybe String -> Handler LogfileParsingResponse
applyLogfileParserByName dbConfig parserName maybeTarget =
  case validatedParams of
    Right (validParserName, validTarget) -> do
      response <- liftIO $ Orchestration.applyLogfileParserByName dbConfig validParserName validTarget

      return response

    Left problem ->
        throwError $ err400
          { errBody = encode problem
          , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
          }

    where validatedParams = ValidationOrchestration.validateLogfileParsingUrlRequest parserName maybeTarget


applyLogfileParserHandler :: LogfileParsingRequest -> Handler LogfileParsingResponse
applyLogfileParserHandler request =
  case validatedRequest of
    Right validRequest -> do
      let response = Orchestration.applyLogfileParser request

      return response

    Left problem ->
      throwError $ err400
        { errBody = encode problem
        , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
        }


  where validatedRequest = ValidationOrchestration.validateLogfileParsingRequest request


applyLogfileParserToFileHandler :: Configs.FileDbConfig -> LogfileParsingFileRequest -> Handler LogfileParsingResponse
applyLogfileParserToFileHandler dbConfig request =
  case validatedRequest of
    Right validRequest -> do
      response <- liftIO $ Orchestration.applyLogfileParserToFile dbConfig request

      return response

    Left problem ->
      throwError $ err400
        { errBody = encode problem
        , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
        }


  where validatedRequest = ValidationOrchestration.validateLogfileParsingFileRequest request


readAllElementaryParsersHandler ::  Configs.FileDbConfig -> Handler [RM.ElementaryParser]
readAllElementaryParsersHandler dbConfig = do
  parsers <- liftIO $ Orchestration.existingElementaryParsers dbConfig
  let response = map CM.toRestElementaryParser parsers
  
  return response


saveParserHandler ::  Configs.FileDbConfig -> ElementaryParser -> Handler NoContent
saveParserHandler dbConfig parser =
  case validatedParser of
    Right validParser -> do
        _ <- liftIO $ Orchestration.createElementaryParser dbConfig validParser

        return NoContent

    Left problem ->
        throwError $ err400
          { errBody = encode problem
          , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
          }


  where validatedParser = ValidationOrchestration.validateElementaryParserToCreate parser


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
