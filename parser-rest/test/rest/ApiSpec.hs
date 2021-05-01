{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiSpec
( spec
) where


import Test.Hspec

import Data.Aeson
import Data.Aeson.TH
import qualified Data.List as List
import qualified Data.Maybe as MaybeModule
import qualified Data.Either as EitherModule
import Data.UUID
import Network.Wai.Handler.Warp as Warp
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)


import qualified Network.HTTP.Client              as HttpClient
import qualified Network.Wai.Handler.Warp         as Warp
import           Servant
import           Servant.Multipart
import qualified Servant.Client as ServC

import qualified ElementaryParserFileDb as ElemDb -- For initialising data
import qualified LogfileParserFileDb as LogfileDb -- For initialising data
import qualified Configs as Configs
import qualified Api as Api

import qualified ModelMapping as MM
import qualified RestParserModels as RM



createDbFiles :: IO ()
createDbFiles = do
    createDirectoryIfMissing True dbBasePath
    writeFile (dbBasePath ++ elementaryParsersDbName) $ show ([] :: [Int])
    writeFile (dbBasePath ++ logfileParsersDbName) $ show ([] :: [Int])


clearDbFiles :: IO ()
clearDbFiles = removeDirectoryRecursive dbBasePath



spec :: Spec
spec =  before_ createDbFiles $
        before_ createElementaryParsers $
        after_ clearDbFiles $ do
        around withUserApp $ do
          -- let getNames = ServC.client Api.api -- Change back to Proxy...
          let client = ServC.client Api.api
          baseUrl <- runIO $ ServC.parseBaseUrl "http://localhost"
          manager <- runIO $ HttpClient.newManager HttpClient.defaultManagerSettings

          let clientEnv port = ServC.mkClientEnv manager (baseUrl {ServC.baseUrlPort = port})


          describe "api" $ do
            describe "building-blocks" $ do
              describe "GET elementary parser IDs as JSON" $ do
                it "returns the IDs and names of the parsers added by before_ as a list" $ \port -> do
                  result <- ServC.runClientM
                              (getElementaryParserIds client)
                              (clientEnv port)

                  let parserNames = map (\(RM.ElementaryParser name _ _) -> name) initialElementaryParsers

                  result `shouldSatisfy` matchesElementaryParsingNames parserNames


              describe "GET elementary parsers as JSON" $ do
                it "returns the names of the parsers added by before_ as a list" $ \port -> do
                  result <- ServC.runClientM
                              (getElementaryParsers client)
                              (clientEnv port)
                  result `shouldBe` Right initialElementaryParsers


              describe "POST parser as JSON creates the parser and" $ do
                it "returns NoContent" $ \port -> do
                  let parser = RM.ElementaryParser "newLoglevelParser" (RM.ParsingOptions { RM.keepResult = True }) (RM.OneOf ["TRACE", "DEBUG", "INFO", "ERROR"])
                  creationResult <- ServC.runClientM
                              (createElementaryParser client parser)
                              (clientEnv port)
                  creationResult `shouldBe` Right NoContent

                  getResult <- ServC.runClientM
                              (getElementaryParsers client)
                              (clientEnv port)
                  getResult `shouldBe` Right (parser : initialElementaryParsers)


              describe "GET parsing response for existing parser via URL params" $ do
                it "returns the parsing response" $ \port -> do
                  eitherIds <- ServC.runClientM
                              (getElementaryParserIds client)
                              (clientEnv port)

                  eitherIds `shouldSatisfy` EitherModule.isRight

                  case eitherIds of
                    Left err ->
                      True `shouldBe` False

                    Right ids -> do
                      let parserName = "loglevelParser"
                      let maybeParserId = List.find (matchesElementaryName parserName) ids

                      maybeParserId `shouldSatisfy` MaybeModule.isJust

                      let RM.ElementaryParserId uuid _ = MaybeModule.fromJust maybeParserId
                      let target = Just "INCIDENT some message"

                      result <- ServC.runClientM
                                  (applyElementaryParserById client uuid target)
                                  (clientEnv port)

                      result `shouldBe` (Right $ RM.ElementaryParsingResponse
                                                  parserName
                                                  (RM.OneOfResult "INCIDENT"))



              describe "POST parser and target as JSON applies the parser to the target and" $ do
                it "returns the parsing response" $ \port -> do
                  let parsingRequest = RM.ElementaryParsingRequest
                                          "DEBUG some message"
                                          (RM.ElementaryParser "newLoglevelParser" (RM.ParsingOptions { RM.keepResult = True }) (RM.OneOf ["TRACE", "DEBUG", "INFO", "ERROR"]))

                  result <- ServC.runClientM
                              (applyElementaryParser client parsingRequest)
                              (clientEnv port)

                  result `shouldBe` (Right $ RM.ElementaryParsingResponse
                                              "newLoglevelParser"
                                              (RM.OneOfResult "DEBUG"))



          before_ createLogfileParsers $
            describe "logfile" $ do
              describe "GET logfile parser names as JSON" $ do
                it "returns a list of parser names" $ \port -> do
                  result <- ServC.runClientM
                              (getLogfileParserIds client)
                              (clientEnv port)
                  result `shouldSatisfy` matchesLogfileParsingNames ["myLogfileParser"]


              describe "POST parser as JSON creates the parser and" $ do
                it "returns NoContent" $ \port -> do
                  let parser = RM.LogfileParser
                                  "newLogfileParser"
                                  [ RM.NamedElementaryParser "loglevel" oneOfParser
                                  , RM.NamedElementaryParser "space" spaceParser
                                  , RM.NamedElementaryParser "message" matchUntilEndParser
                                  ]

                  creationResult <- ServC.runClientM
                              (createLogfileParser client parser)
                              (clientEnv port)
                  creationResult `shouldBe` Right NoContent

                  getResult <- ServC.runClientM
                              (getLogfileParserIds client)
                              (clientEnv port)
                  getResult `shouldSatisfy` matchesLogfileParsingNames ["newLogfileParser", "myLogfileParser"]


              describe "GET parsing response for existing parser via URL params" $ do
                it "returns the parsing response" $ \port -> do
                  let parserName = "myLogfileParser"
                  let target = Just "INCIDENT 2021.02.13 16-13 some stuff before id <correlationId>asg-qwta123-fd</correlationId> error message"

                  result <- ServC.runClientM
                              (applyLogfileParserByName client parserName target)
                              (clientEnv port)

                  result `shouldBe` (Right $ RM.LogfileParsingResponse [
                                              [ RM.ElementaryParsingResponse "LogLevel" (RM.OneOfResult "INCIDENT")
                                              , RM.ElementaryParsingResponse "space" (RM.OneOfResult " ")
                                              , RM.ElementaryParsingResponse "LogDate" (RM.OneOfResult "2021-02-13")
                                              , RM.ElementaryParsingResponse "space" (RM.OneOfResult " ")
                                              , RM.ElementaryParsingResponse "LogTime" (RM.OneOfResult "16:13:00")
                                              , RM.ElementaryParsingResponse "space" (RM.OneOfResult " ")
                                              , RM.ElementaryParsingResponse "UntilCorrelationId" (RM.OneOfResult "some stuff before id <correlationId>")
                                              , RM.ElementaryParsingResponse "CorrelationId" (RM.OneOfResult "asg-qwta123-fd")
                                              , RM.ElementaryParsingResponse "CorrelationIdEndTag" (RM.OneOfResult "</correlationId>")
                                              , RM.ElementaryParsingResponse "forSpace" (RM.OneOfResult " ")
                                              , RM.ElementaryParsingResponse "restOfTheMessage" (RM.OneOfResult "error message")
                                              ]])



              describe "POST parser and target as JSON applies the parser to the target and" $ do
                it "returns a list of parser names" $ \port -> do
                  let parsingRequest = RM.LogfileParsingRequest
                                  "ERROR some message describing the error\n\
                                  \INFO some info"
                                  (RM.LogfileParser
                                    "newLogfileParser"
                                    [ RM.NamedElementaryParser "loglevel" oneOfParser
                                    , RM.NamedElementaryParser "space" spaceParser
                                    , RM.NamedElementaryParser "message" matchUntilEndParser
                                    ])

                  result <- ServC.runClientM
                              (applyLogfileParser client parsingRequest)
                              (clientEnv port)

                  result `shouldBe` (Right $ RM.LogfileParsingResponse
                                              [ [ RM.ElementaryParsingResponse "loglevel" (RM.OneOfResult "ERROR")
                                                , RM.ElementaryParsingResponse "space" (RM.OneOfResult " ")
                                                , RM.ElementaryParsingResponse "message" (RM.OneOfResult "some message describing the error")
                                                ]
                                              , [ RM.ElementaryParsingResponse "loglevel" (RM.OneOfResult "INFO")
                                                , RM.ElementaryParsingResponse "space" (RM.OneOfResult " ")
                                                , RM.ElementaryParsingResponse "message" (RM.OneOfResult "some info")
                                                ]
                                              ])


              describe "POST parser and target as JSON applies the parser to the target and" $ do
                it "returns a list of parser names" $ \port -> do
                  let path = "foo_bar_baz"
                  let parsingRequest = RM.LogfileParsingFileRequest
                                        "myLogfileParser"
                                        "assets\\sample_logfiles\\test_logfile.log"
                                        --"./assets/sample-logfiles/test_logfile.log"


                  result <- ServC.runClientM
                              (applyLogfileParserToFile client (path, parsingRequest))
                              (clientEnv port)

                  result `shouldBe` (Right $ RM.LogfileParsingResponse [
                                              [ RM.ElementaryParsingResponse "LogLevel" (RM.OneOfResult "INCIDENT")
                                              , RM.ElementaryParsingResponse "space" (RM.OneOfResult " ")
                                              , RM.ElementaryParsingResponse "LogDate" (RM.OneOfResult "2021-02-13")
                                              , RM.ElementaryParsingResponse "space" (RM.OneOfResult " ")
                                              , RM.ElementaryParsingResponse "LogTime" (RM.OneOfResult "16:13:00")
                                              , RM.ElementaryParsingResponse "space" (RM.OneOfResult " ")
                                              , RM.ElementaryParsingResponse "UntilCorrelationId" (RM.OneOfResult "some stuff before id <correlationId>")
                                              , RM.ElementaryParsingResponse "CorrelationId" (RM.OneOfResult "asg-qwta123-fd")
                                              , RM.ElementaryParsingResponse "CorrelationIdEndTag" (RM.OneOfResult "</correlationId>")
                                              , RM.ElementaryParsingResponse "forSpace" (RM.OneOfResult " ")
                                              , RM.ElementaryParsingResponse "restOfTheMessage" (RM.OneOfResult "error message")
                                              ]])




withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action =
    Warp.testWithApplication (pure $ Api.app fileDbConfig) action


createElementaryParsers :: IO ()
createElementaryParsers = mapM_
                            (ElemDb.save fileDbConfig)
                            (reverse (map (MM.toDbElementaryParser . MM.fromRestElementaryParser) initialElementaryParsers))

  where dbPath = dbBasePath ++ elementaryParsersDbName


createLogfileParsers :: IO ()
createLogfileParsers = mapM_
                        (LogfileDb.save fileDbConfig)
                        (map (MM.toDbLogfileParser . MM.fromRestLogfileParser) [logfileParser])


fileDbConfig :: Configs.FileDbConfig
fileDbConfig = Configs.FileDbConfig
                  (T.pack $ dbBasePath ++ elementaryParsersDbName)
                  (T.pack $ dbBasePath ++ logfileParsersDbName)


initialElementaryParsers :: [RM.ElementaryParser]
initialElementaryParsers =  [ oneOfParser
                            , timeParser
                            , dateParser
                            , spaceParser
                            , charactersParser
                            , matchUntilIncludedParser
                            , matchUntilExcludedParser
                            , matchForParser
                            , matchUntilEndParser
                            ]


oneOfParser :: RM.ElementaryParser
oneOfParser = RM.ElementaryParser "loglevelParser" (RM.ParsingOptions { RM.keepResult = True }) (RM.OneOf ["INFO", "INCIDENT", "ERROR"])

timeParser :: RM.ElementaryParser
timeParser = RM.ElementaryParser "dashedTimeParser" (RM.ParsingOptions { RM.keepResult = True }) (RM.Time "HH-MM")

dateParser :: RM.ElementaryParser
dateParser = RM.ElementaryParser "dottedDateParser" (RM.ParsingOptions { RM.keepResult = True }) (RM.Date "YYYY.MM.DD")

spaceParser :: RM.ElementaryParser
spaceParser = RM.ElementaryParser "spaceParser" (RM.ParsingOptions { RM.keepResult = True }) (RM.Characters " ")

charactersParser :: RM.ElementaryParser
charactersParser = RM.ElementaryParser "correlationIdEndTag" (RM.ParsingOptions { RM.keepResult = True }) (RM.Characters "</correlationId>")

matchUntilIncludedParser :: RM.ElementaryParser
matchUntilIncludedParser = RM.ElementaryParser "untilCorrelationId" (RM.ParsingOptions { RM.keepResult = True }) (RM.MatchUntilIncluded "<correlationId>")

matchUntilExcludedParser :: RM.ElementaryParser
matchUntilExcludedParser = RM.ElementaryParser "correlationId" (RM.ParsingOptions { RM.keepResult = True }) (RM.MatchUntilExcluded "</correlationId>")

matchForParser :: RM.ElementaryParser
matchForParser = RM.ElementaryParser "for1Space" (RM.ParsingOptions { RM.keepResult = True }) (RM.MatchFor 1)

matchUntilEndParser :: RM.ElementaryParser
matchUntilEndParser = RM.ElementaryParser "matchUntilEnd" (RM.ParsingOptions { RM.keepResult = True }) RM.MatchUntilEnd


logfileParser :: RM.LogfileParser
logfileParser = RM.LogfileParser
                  "myLogfileParser"
                  [ RM.NamedElementaryParser "LogLevel" oneOfParser
                  , RM.NamedElementaryParser "space" spaceParser
                  , RM.NamedElementaryParser "LogDate" dateParser
                  , RM.NamedElementaryParser "space" spaceParser
                  , RM.NamedElementaryParser "LogTime" timeParser
                  , RM.NamedElementaryParser "space" spaceParser
                  , RM.NamedElementaryParser "UntilCorrelationId" matchUntilIncludedParser
                  , RM.NamedElementaryParser "CorrelationId" matchUntilExcludedParser
                  , RM.NamedElementaryParser "CorrelationIdEndTag" charactersParser
                  , RM.NamedElementaryParser "forSpace" matchForParser
                  , RM.NamedElementaryParser "restOfTheMessage" matchUntilEndParser
                  ]
-- data Config = Config
--   { fileDbConfig :: FileDbConfig
--   , apiConfig :: APIConfig
--   }
--
--
--
-- data FileDbConfig = FileDbConfig
--   { elementaryParserPath :: T.Text
--   , logfileParserPath :: T.Text
--   } deriving (Show)


dbBasePath :: FilePath
dbBasePath = "assets/test_db"

elementaryParsersDbName :: String
elementaryParsersDbName = "/parsers.txt"

logfileParsersDbName :: String
logfileParsersDbName = "/logfile_parsers.txt"


getLogfileParserIds :: ServC.Client ServC.ClientM Api.API -> ServC.ClientM [RM.LogfileParserId]
getLogfileParserIds (( parserNames :<|> _ ) :<|> (_)) = parserNames

createLogfileParser :: ServC.Client ServC.ClientM Api.API -> (RM.CreateLogfileParserRequest -> ServC.ClientM NoContent)
createLogfileParser (( _ :<|> createParser :<|> _) :<|> (_)) = createParser

applyLogfileParserByName :: ServC.Client ServC.ClientM Api.API -> (String -> Maybe String -> ServC.ClientM RM.LogfileParsingResponse)
applyLogfileParserByName (( _ :<|> _ :<|> applyByName :<|> _) :<|> (_)) = applyByName

applyLogfileParser :: ServC.Client ServC.ClientM Api.API -> (RM.LogfileParsingRequest -> ServC.ClientM RM.LogfileParsingResponse)
applyLogfileParser (( _ :<|> _ :<|> _ :<|> applyParser :<|> _) :<|> (_)) = applyParser

applyLogfileParserToFile :: ServC.Client ServC.ClientM Api.API -> ((ByteString, RM.LogfileParsingFileRequest) -> ServC.ClientM RM.LogfileParsingResponse)
applyLogfileParserToFile (( _ :<|> _ :<|> _ :<|> _ :<|> applyParser) :<|> (_)) = applyParser


getElementaryParserIds :: ServC.Client ServC.ClientM Api.API -> ServC.ClientM [RM.ElementaryParserId]
getElementaryParserIds ((_) :<|> (getParserIds :<|> _)) = getParserIds

getElementaryParsers :: ServC.Client ServC.ClientM Api.API -> ServC.ClientM [RM.ElementaryParser]
getElementaryParsers ((_) :<|> ( _ :<|> getParsers :<|> _)) = getParsers

createElementaryParser :: ServC.Client ServC.ClientM Api.API -> (RM.ElementaryParser -> ServC.ClientM NoContent)
createElementaryParser ((_) :<|> ( _ :<|> _ :<|> createParser :<|> _)) = createParser

applyElementaryParserById :: ServC.Client ServC.ClientM Api.API -> (UUID -> Maybe String -> ServC.ClientM RM.ElementaryParsingResponse)
applyElementaryParserById ((_) :<|> ( _ :<|> _ :<|> _ :<|> applyById :<|> _)) = applyById

applyElementaryParser :: ServC.Client ServC.ClientM Api.API -> (RM.ElementaryParsingRequest -> ServC.ClientM RM.ElementaryParsingResponse)
applyElementaryParser ((_) :<|> ( _ :<|> _ :<|> _ :<|> _ :<|> applyParser)) = applyParser




matchesLogfileParsingNames :: [String] -> Either a [RM.LogfileParserId] -> Bool
matchesLogfileParsingNames names (Right ids) = names == map extractLogfileName ids
matchesLogfileParsingNames _ _ = False

extractLogfileName :: RM.LogfileParserId -> String
extractLogfileName (RM.LogfileParserId _ name) = name

matchesElementaryParsingNames :: [String] -> Either a [RM.ElementaryParserId] -> Bool
matchesElementaryParsingNames names (Right ids) = names == map extractElementaryName ids
matchesElementaryParsingNames _ _ = False

extractElementaryName :: RM.ElementaryParserId -> String
extractElementaryName (RM.ElementaryParserId _ name) = name

matchesElementaryName :: String -> RM.ElementaryParserId -> Bool
matchesElementaryName parserName (RM.ElementaryParserId _ name) = parserName == name


instance ToJSON RM.ElementaryParsingRequest where
  toJSON (RM.ElementaryParsingRequest target parser) =
    object [ "target" .= target, "parser" .= parser ]


instance ToJSON RM.LogfileParsingRequest where
  toJSON (RM.LogfileParsingRequest target parser) =
    object [ "target" .= target, "parser" .= parser ]


instance ToMultipart Tmp RM.LogfileParsingFileRequest where
  toMultipart (RM.LogfileParsingFileRequest name logfile) =
      MultipartData [ Input "name" (T.pack name) ]
                    [ FileData
                        "logfile"
                        (T.pack logfile)
                        "text/plain"
                        logfile
                    ]


instance ToJSON RM.LogfileParser where
  toJSON (RM.LogfileParser name parsers) =
    object [ "name" .= name, "parsers" .= parsers]


instance ToJSON RM.NamedElementaryParser where
  toJSON (RM.NamedElementaryParser name parser) =
    object [ "name" .= name, "parser" .= parser]


instance FromJSON RM.ElementaryParsingResponse where
  parseJSON (Object o) =
    do maybeResult <- o .:? "result"
       case maybeResult of
         Just (String result) ->
            RM.ElementaryParsingResponse <$> o .: "name" <*> (fmap RM.OneOfResult (o .: "result"))

         Nothing ->
            RM.ElementaryParsingResponse <$> o .: "name" <*> (fmap RM.ParsingError (o .: "error"))


instance FromJSON RM.ElementaryParserId where
  parseJSON (Object o) =
    RM.ElementaryParserId <$> o .: "id" <*> o .: "name"


instance FromJSON RM.LogfileParserId where
  parseJSON (Object o) =
    RM.LogfileParserId <$> o .: "id" <*> o .: "name"


instance FromJSON RM.LogfileParsingResponse where
  parseJSON (Object o) =
    do parserType <- o .:? "result"
       let maybeResponse = fmap RM.LogfileParsingResponse parserType
       case maybeResponse of
          Just response ->
            return response

          Nothing ->
            RM.LogfileParsingError <$> o .: "error"
