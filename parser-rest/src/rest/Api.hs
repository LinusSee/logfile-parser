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
import Data.UUID (UUID)
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

import HttpErrors (Problem(..))
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
                     Get '[JSON] [RM.LogfileParserId]
                :<|> ReqBody '[JSON] RM.CreateLogfileParserRequest :> PostCreated '[JSON] NoContent
                :<|> "apply" :> Capture "id" UUID :> QueryParam "target" String :> Get '[JSON] RM.LogfileParsingResponse
                :<|> "apply" :> ReqBody '[JSON] RM.LogfileParsingRequest :> Post '[JSON] RM.LogfileParsingResponse
                :<|> "apply" :> "file" :> MultipartForm Tmp RM.LogfileParsingFileRequest :> Post '[JSON] RM.LogfileParsingResponse
                )
            )
            :<|>
            ( "building-blocks" :> "complex" :>
                (    "ids" :> Get '[JSON] [RM.ElementaryParserId]
                :<|> Get '[JSON] [RM.ElementaryParser]
                :<|> ReqBody '[JSON] RM.ElementaryParser :> PostCreated '[JSON] NoContent
                :<|> "apply" :> Capture "id" UUID :> QueryParam "target" String :> Get '[JSON] RM.ElementaryParsingResponse
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
  (    getLogfileParserIds dbConfig
  :<|> saveLogfileParserHandler dbConfig
  :<|> applyLogfileParserById dbConfig
  :<|> applyLogfileParserHandler
  :<|> applyLogfileParserToFileHandler dbConfig
  )
  :<|> readAllElementaryParserIdsHandler dbConfig
  :<|> readAllElementaryParsersHandler dbConfig
  :<|> saveParserHandler dbConfig
  :<|> applyParserByIdHandler dbConfig
  :<|> applyParserHandler


getLogfileParserIds ::  Configs.FileDbConfig -> Handler [RM.LogfileParserId]
getLogfileParserIds dbConfig = do
  result <- liftIO $ Orchestration.existingLogfileParserIds dbConfig
  let response = map MM.toRestLogfileParserId result

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


applyLogfileParserById ::  Configs.FileDbConfig -> UUID -> Maybe String -> Handler RM.LogfileParsingResponse
applyLogfileParserById dbConfig uuid maybeTarget =
  case validatedParams of
    Right validTarget -> do
      result <- liftIO $ Orchestration.applyLogfileParserById dbConfig (uuid, validTarget)
      let response = MM.toRestLogfileParsingResponse result

      return response

    Left problem ->
        throwError $ err400
          { errBody = encode problem
          , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
          }

    where validatedParams = ValidationOrchestration.validateParsingTarget maybeTarget


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
applyLogfileParserToFileHandler dbConfig (RM.LogfileParsingFileRequest maybeUuid filePath) =
  case maybeUuid of
    Just uuid -> do
      result <- liftIO $ Orchestration.applyLogfileParserToFile dbConfig (uuid, filePath)
      let response = MM.toRestLogfileParsingResponse result

      return response

    Nothing -> do
      let problem = Problem { problemType = "http://localhost/problems/validation-error"
                            , title = "Failed to validate request parameters."
                            , status = 400
                            , detail = "UUID was in an incorrect format."
                            , errors = []
                            }

      throwError $ err400
        { errBody = encode problem
        , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
        }


readAllElementaryParserIdsHandler :: Configs.FileDbConfig -> Handler [RM.ElementaryParserId]
readAllElementaryParserIdsHandler dbConfig = do
  ids <- liftIO $ Orchestration.existingElementaryParserIds dbConfig
  let response = map MM.toRestElementaryParserId ids

  return response


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


applyParserByIdHandler ::  Configs.FileDbConfig -> UUID -> Maybe String -> Handler RM.ElementaryParsingResponse
applyParserByIdHandler dbConfig uuid maybeTarget =
  case validatedTarget of
    Right validTarget -> do
      result <- liftIO $ Orchestration.applyElementaryParserById dbConfig uuid validTarget
      let response = MM.toRestElementaryParsingResponse result

      return response

    Left problem ->
      throwError $ err400
        { errBody = encode problem
        , errHeaders = [((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))]
        }

  where validatedTarget = ValidationOrchestration.validateParsingTarget maybeTarget



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
