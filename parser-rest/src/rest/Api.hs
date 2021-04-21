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


import qualified Configs as Configs
import qualified ParsingOrchestration as Orchestration
import qualified ValidationOrchestration as ValidationOrchestration

import qualified ModelMapping as MM
import qualified RestParserModels as RM


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
                :<|> ReqBody '[JSON] RM.CreateLogfileParserRequest :> PostCreated '[JSON] NoContent
                :<|> "apply" :> Capture "parserName" String :> QueryParam "target" String :> Get '[JSON] RM.LogfileParsingResponse
                :<|> "apply" :> ReqBody '[JSON] RM.LogfileParsingRequest :> Post '[JSON] RM.LogfileParsingResponse
                :<|> "apply" :> "file" :> MultipartForm Tmp RM.LogfileParsingFileRequest :> Post '[JSON] RM.LogfileParsingResponse
                )
            )
            :<|>
            ( "building-blocks" :> "complex" :>
                (    Get '[JSON] [RM.ElementaryParser]
                :<|> ReqBody '[JSON] RM.ElementaryParser :> PostCreated '[JSON] NoContent
                :<|> "apply" :> Capture "parserName" String :> QueryParam "target" String :> Get '[JSON] RM.ElementaryParsingResponse
                :<|> "apply" :> ReqBody '[JSON] RM.ElementaryParsingRequest :> Post '[JSON] RM.ElementaryParsingResponse
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


saveLogfileParserHandler ::  Configs.FileDbConfig -> RM.CreateLogfileParserRequest -> Handler NoContent
saveLogfileParserHandler dbConfig request =
  case validatedRequest of
    Right validRequest -> do
        _ <- liftIO $ Orchestration.createLogfileParser dbConfig (MM.fromRestCreateLogfileParserRequest validRequest)

        return NoContent

    Left problem ->
        throwError $ err400
          { errBody = encode problem
          , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
          }

  where validatedRequest = ValidationOrchestration.validateCreateLogfileParserRequest request


applyLogfileParserByName ::  Configs.FileDbConfig -> String -> Maybe String -> Handler RM.LogfileParsingResponse
applyLogfileParserByName dbConfig parserName maybeTarget =
  case validatedParams of
    Right (validParserName, validTarget) -> do
      result <- liftIO $ Orchestration.applyLogfileParserByName dbConfig (validParserName, validTarget)
      let response = MM.toRestLogfileParsingResponse result

      return response

    Left problem ->
        throwError $ err400
          { errBody = encode problem
          , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
          }

    where validatedParams = ValidationOrchestration.validateLogfileParsingUrlRequest parserName maybeTarget


applyLogfileParserHandler :: RM.LogfileParsingRequest -> Handler RM.LogfileParsingResponse
applyLogfileParserHandler request =
  case validatedRequest of
    Right validRequest -> do
      let result = Orchestration.applyLogfileParser (MM.fromRestLogfileParsingRequest request)
      let response = MM.toRestLogfileParsingResponse result

      return response

    Left problem ->
      throwError $ err400
        { errBody = encode problem
        , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
        }


  where validatedRequest = ValidationOrchestration.validateLogfileParsingRequest request


applyLogfileParserToFileHandler :: Configs.FileDbConfig -> RM.LogfileParsingFileRequest -> Handler RM.LogfileParsingResponse
applyLogfileParserToFileHandler dbConfig request =
  case validatedRequest of
    Right validRequest -> do
      result <- liftIO $ Orchestration.applyLogfileParserToFile dbConfig (MM.fromRestLogfileParsingFileRequest validRequest)
      let response = MM.toRestLogfileParsingResponse result

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
  let response = map MM.toRestElementaryParser parsers

  return response


saveParserHandler ::  Configs.FileDbConfig -> RM.ElementaryParser -> Handler NoContent
saveParserHandler dbConfig parser =
  case validatedParser of
    Right validParser -> do
        _ <- liftIO $ Orchestration.createElementaryParser dbConfig (MM.fromRestElementaryParser validParser)

        return NoContent

    Left problem ->
        throwError $ err400
          { errBody = encode problem
          , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
          }


  where validatedParser = ValidationOrchestration.validateElementaryParserToCreate parser


applyParserByNameHandler ::  Configs.FileDbConfig -> String -> Maybe String -> Handler RM.ElementaryParsingResponse
applyParserByNameHandler dbConfig parserName maybeTarget =
  case validatedParams of
    Right (validParserName, validTarget) -> do
      result <- liftIO $ Orchestration.applyElementaryParserByName dbConfig validParserName validTarget
      let response = MM.toRestElementaryParsingResponse result

      return response

    Left problem ->
      throwError $ err400
        { errBody = encode problem
        , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
        }

  where validatedParams = ValidationOrchestration.validateParsingUrlRequest parserName maybeTarget



applyParserHandler :: RM.ElementaryParsingRequest -> Handler RM.ElementaryParsingResponse
applyParserHandler request =
  case validatedRequest of
    Right validRequest -> do
      let result = Orchestration.applyElementaryParser (MM.fromElementaryParsingRequest validRequest)
      let response = MM.toRestElementaryParsingResponse result

      return response

    Left problem ->
      throwError $ err400
        { errBody = encode problem
        , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
        }

  where validatedRequest = ValidationOrchestration.validateParsingRequest request
