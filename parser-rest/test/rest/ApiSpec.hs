module ApiSpec
( spec
) where


import Test.Hspec

import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import qualified Data.Text as T

import CustomParsers (ElementaryParser (..))
import ElementaryParserFileDb (save) -- For initialising data
import qualified Configs as Configs


createDbFiles :: IO ()
createDbFiles = do
    createDirectoryIfMissing True dbBasePath
    writeFile (dbBasePath ++ elementaryParsersDbName) $ show ([] :: [Int])
    writeFile (dbBasePath ++ logfileParsersDbName) $ show ([] :: [Int])


clearDbFiles :: IO ()
clearDbFiles = removeDirectoryRecursive dbBasePath



spec :: Spec
spec = before_ createDbFiles $
       after_ clearDbFiles $ do
         describe "api" $ do
           it "returns" $ do
             pending




createElementaryParsers :: IO ()
createElementaryParsers = mapM_ (save fileDbConfig) parsers

  where dbPath = dbBasePath ++ elementaryParsersDbName
        parsers = [ oneOfParser
                  , timeParser
                  , matchUntilIncludedParser
                  , matchUntilExcludedParser
                  , matchUntilEndParser
                  ]


fileDbConfig :: Configs.FileDbConfig
fileDbConfig = Configs.FileDbConfig
                  (T.pack $ dbBasePath ++ elementaryParsersDbName)
                  (T.pack $ dbBasePath ++ logfileParsersDbName)

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
