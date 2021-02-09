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





-- import           Control.Concurrent.MVar
-- import           Control.Exception                (bracket)
-- import           Data.Text                        (Text, unpack)
-- import           GHC.Generics
import qualified Network.HTTP.Client              as HttpClient
-- import           Network.HTTP.Types
-- import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp
--
import           Servant
-- import           Servant.Client
-- import           Servant.Server
-- import           Servant.QuickCheck
-- import           Servant.QuickCheck.Internal (serverDoesntSatisfy)
--
-- import           Test.Hspec
-- import           Test.Hspec.Wai         hiding (pending)
-- import           Test.Hspec.Wai.Matcher
-- import Servant
import qualified Servant.Client as ServC




import CustomParsers ( ElementaryParser (..)
                     , LogfileParsingResponse (..)
                     , ParsingResponse (..)
                     , ParsingResult (..)
                     , CreateLogfileParserRequest (..)
                     , LogfileParsingRequest (..)
                     , NamedParser (..)
                     , ParsingRequest (..)
                     )
import ElementaryParserFileDb (save) -- For initialising data
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
              describe "GET parser names as JSON" $ do
                it "returns the names of the parsers added by before_ as a list" $ \port -> do
                  result <- ServC.runClientM
                              (getElementaryParsers client)
                              (clientEnv port)
                  result `shouldBe` Right initialElementaryParsers
                  pending


          describe "logfile" $ do
            it "returns" $ \port -> do
              pending





withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action =
    Warp.testWithApplication (pure $ Api.app fileDbConfig) action


createElementaryParsers :: IO ()
createElementaryParsers = mapM_
                            (save fileDbConfig)
                            (reverse initialElementaryParsers)

  where dbPath = dbBasePath ++ elementaryParsersDbName


fileDbConfig :: Configs.FileDbConfig
fileDbConfig = Configs.FileDbConfig
                  (T.pack $ dbBasePath ++ elementaryParsersDbName)
                  (T.pack $ dbBasePath ++ logfileParsersDbName)


initialElementaryParsers :: [ElementaryParser]
initialElementaryParsers =  [ oneOfParser
                            , timeParser
                            , matchUntilIncludedParser
                            , matchUntilExcludedParser
                            , matchUntilEndParser
                            ]


oneOfParser :: ElementaryParser
oneOfParser = OneOf "loglevelParser" ["INFO", "INCIDENT", "ERROR"]

timeParser :: ElementaryParser
timeParser = Time "dashedTimeParser" "HH-MM"

matchUntilIncludedParser :: ElementaryParser
matchUntilIncludedParser = MatchUntilIncluded "untilCorrelationId" "<correlationId>"

matchUntilExcludedParser :: ElementaryParser
matchUntilExcludedParser = MatchUntilExcluded "correlationId" "</correlationId>"

matchUntilEndParser :: ElementaryParser
matchUntilEndParser = MatchUntilEnd "matchUntilEnd"

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
