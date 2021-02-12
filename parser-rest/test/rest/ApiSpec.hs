{-# LANGUAGE OverloadedStrings #-}

module ApiSpec
( spec
) where


import Test.Hspec

import Data.Aeson
import Data.Aeson.TH
import Network.Wai.Handler.Warp as Warp
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)






import qualified Network.HTTP.Client              as HttpClient
import qualified Network.Wai.Handler.Warp         as Warp
import           Servant
import qualified Servant.Client as ServC




import CustomParsers ( ElementaryParser (..)
                     , LogfileParser (..)
                     , LogfileParsingResponse (..)
                     , ParsingResponse (..)
                     , ParsingResult (..)
                     , CreateLogfileParserRequest (..)
                     , LogfileParsingRequest (..)
                     , NamedParser (..)
                     , ParsingRequest (..)
                     )
import qualified ElementaryParserFileDb as ElemDb -- For initialising data
import qualified LogfileParserFileDb as LogfileDb -- For initialising data
import qualified Configs as Configs
import qualified Api as Api


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
              describe "GET elementary parsers as JSON" $ do
                it "returns the names of the parsers added by before_ as a list" $ \port -> do
                  result <- ServC.runClientM
                              (getElementaryParsers client)
                              (clientEnv port)
                  result `shouldBe` Right initialElementaryParsers


              describe "POST parser as JSON creates the parser and" $ do
                it "returns NoContent" $ \port -> do
                  let parser = OneOf "newLoglevelParser" ["TRACE", "DEBUG", "INFO", "ERROR"]
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
                  let parserName = "loglevelParser"
                  let target = Just "INCIDENT some message"

                  result <- ServC.runClientM
                              (applyElementaryParserByName client parserName target)
                              (clientEnv port)

                  result `shouldBe` (Right $ ParsingResponse
                                              parserName
                                              (OneOfResult "INCIDENT"))


              describe "POST parser and target as JSON applies the parser to the target and" $ do
                it "returns the parsing response" $ \port -> do
                  let parsingRequest = ParsingRequest
                                          "DEBUG some message"
                                          (OneOf "newLoglevelParser" ["TRACE", "DEBUG", "INFO", "ERROR"])

                  result <- ServC.runClientM
                              (applyElementaryParser client parsingRequest)
                              (clientEnv port)

                  result `shouldBe` (Right $ ParsingResponse
                                              "newLoglevelParser"
                                              (OneOfResult "DEBUG"))



          before_ createLogfileParsers $
            describe "logfile" $ do
              describe "GET logfile parser names as JSON" $ do
                it "returns a list of parser names" $ \port -> do
                  pending


              describe "POST parser as JSON creates the parser and" $ do
                it "returns NoContent" $ \port -> do
                  pending


              describe "GET parsing response for existing parser via URL params" $ do
                it "returns the parsing response" $ \port -> do
                  pending


              describe "POST parser and target as JSON applies the parser to the target and" $ do
                it "returns a list of parser names" $ \port -> do
                  pending





withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action =
    Warp.testWithApplication (pure $ Api.app fileDbConfig) action


createElementaryParsers :: IO ()
createElementaryParsers = mapM_
                            (ElemDb.save fileDbConfig)
                            (reverse initialElementaryParsers)

  where dbPath = dbBasePath ++ elementaryParsersDbName


createLogfileParsers :: IO ()
createLogfileParsers = mapM_
                        (LogfileDb.save fileDbConfig)
                        [logfileParser]


fileDbConfig :: Configs.FileDbConfig
fileDbConfig = Configs.FileDbConfig
                  (T.pack $ dbBasePath ++ elementaryParsersDbName)
                  (T.pack $ dbBasePath ++ logfileParsersDbName)


initialElementaryParsers :: [ElementaryParser]
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


oneOfParser :: ElementaryParser
oneOfParser = OneOf "loglevelParser" ["INFO", "INCIDENT", "ERROR"]

timeParser :: ElementaryParser
timeParser = Time "dashedTimeParser" "HH-MM"

dateParser :: ElementaryParser
dateParser = Date "dottedDateParser" "YYYY.MM.DD"

spaceParser :: ElementaryParser
spaceParser = Characters "spaceParser" " "

charactersParser :: ElementaryParser
charactersParser = Characters "correlationIdEndTag" "</correlationId>"

matchUntilIncludedParser :: ElementaryParser
matchUntilIncludedParser = MatchUntilIncluded "untilCorrelationId" "<correlationId>"

matchUntilExcludedParser :: ElementaryParser
matchUntilExcludedParser = MatchUntilExcluded "correlationId" "</correlationId>"

matchForParser :: ElementaryParser
matchForParser = MatchFor "for1Space" 1

matchUntilEndParser :: ElementaryParser
matchUntilEndParser = MatchUntilEnd "matchUntilEnd"


logfileParser :: LogfileParser
logfileParser = LogfileParser
                  "myLogfileParser"
                  [ ("LogLevel", oneOfParser)
                  , ("space", spaceParser)
                  , ("LogDate", dateParser)
                  , ("space", spaceParser)
                  , ("LogTime", timeParser)
                  , ("UntilCorrelationId", matchUntilIncludedParser)
                  , ("CorrelationId", matchUntilExcludedParser)
                  , ("CorrelationIdEndTag", charactersParser)
                  , ("forSpace", matchForParser)
                  , ("restOfTheMessage", matchUntilEndParser)
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


getLogfileParserNames :: ServC.Client ServC.ClientM Api.API -> ServC.ClientM [String]
getLogfileParserNames (( parserNames :<|> _ ) :<|> (_)) = parserNames

createLogfileParser :: ServC.Client ServC.ClientM Api.API -> (CreateLogfileParserRequest -> ServC.ClientM NoContent)
createLogfileParser (( _ :<|> createParser :<|> _) :<|> (_)) = createParser

applyLogfileParserByName :: ServC.Client ServC.ClientM Api.API -> (String -> Maybe String -> ServC.ClientM LogfileParsingResponse)
applyLogfileParserByName (( _ :<|> _ :<|> applyByName :<|> _) :<|> (_)) = applyByName

applyLogfileParser :: ServC.Client ServC.ClientM Api.API -> (LogfileParsingRequest -> ServC.ClientM LogfileParsingResponse)
applyLogfileParser (( _ :<|> _ :<|> _ :<|> applyParser) :<|> (_)) = applyParser


getElementaryParsers :: ServC.Client ServC.ClientM Api.API -> ServC.ClientM [ElementaryParser]
getElementaryParsers ((_) :<|> ( getParsers :<|> _)) = getParsers

createElementaryParser :: ServC.Client ServC.ClientM Api.API -> (ElementaryParser -> ServC.ClientM NoContent)
createElementaryParser ((_) :<|> ( _ :<|> createParser :<|> _)) = createParser

applyElementaryParserByName :: ServC.Client ServC.ClientM Api.API -> (String -> Maybe String -> ServC.ClientM ParsingResponse)
applyElementaryParserByName ((_) :<|> ( _ :<|> _ :<|> applyByName :<|> _)) = applyByName

applyElementaryParser :: ServC.Client ServC.ClientM Api.API -> (ParsingRequest -> ServC.ClientM ParsingResponse)
applyElementaryParser ((_) :<|> ( _ :<|> _ :<|> _ :<|> applyParser)) = applyParser



instance ToJSON ParsingRequest where
  toJSON (ParsingRequest target parser) =
    object [ "target" .= target, "parser" .= parser ]


instance ToJSON LogfileParsingRequest where
  toJSON (LogfileParsingRequest target parser) =
    object [ "target" .= target, "parser" .= parser ]


instance ToJSON CreateLogfileParserRequest where
  toJSON (CreateLogfileParserRequest name parsers) =
    object [ "name" .= name, "parsers" .= parsers]


instance ToJSON NamedParser where
  toJSON (NamedParser name parser) =
    object [ "name" .= name, "parser" .= parser]


instance FromJSON ParsingResponse where
  parseJSON (Object o) =
    do maybeResult <- o .:? "result"
       case maybeResult of
         Just (String result) ->
            ParsingResponse <$> o .: "name" <*> (fmap OneOfResult (o .: "result"))

         Nothing ->
            ParsingResponse <$> o .: "name" <*> (fmap ParsingError (o .: "error"))


instance FromJSON LogfileParsingResponse where
  parseJSON (Object o) =
    do parserType <- o .:? "result"
       case parserType of Just (String result) ->
                              LogfileParsingResponse <$> o .: "result"

                          Nothing ->
                              LogfileParsingError <$> o .: "error"
