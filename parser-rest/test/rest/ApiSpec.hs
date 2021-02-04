module ApiSpec
( spec
) where


import Test.Hspec

import Network.Wai.Handler.Warp as Warp
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)





-- import           Control.Concurrent.MVar
-- import           Control.Exception                (bracket)
-- import           Data.Text                        (Text, unpack)
-- import           GHC.Generics
-- import           Network.HTTP.Client       hiding (Proxy)
-- import           Network.HTTP.Types
-- import           Network.Wai
-- import qualified Network.Wai.Handler.Warp         as Warp
--
-- import           Servant
-- import           Servant.Client
-- import           Servant.Server
-- import           Servant.QuickCheck
-- import           Servant.QuickCheck.Internal (serverDoesntSatisfy)
--
-- import           Test.Hspec
-- import           Test.Hspec.Wai         hiding (pending)
-- import           Test.Hspec.Wai.Matcher





import CustomParsers (ElementaryParser (..))
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
        -- around withUserApp $ do
          -- let getNames = client Api.api -- Change back to Proxy...
          -- baseUrl <- runIO $ parseBaseUrl "http://localhost"
          -- manager <- runIO $ newManager defaultManagerSettings
          --
          -- let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})


          describe "api" $ do
            describe "building-blocks" $ do
              describe "GET parser names as JSON" $ do
                it "returns the names of the parsers added by before_ as a list" $ do
                  -- result <- runClientM getNames (clientEnv port)
                  -- result `shouldBe` Right ["asdf"]
                  pending


          describe "logfile" $ do
            it "returns" $ do
              pending





withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action =
    Warp.testWithApplication (pure $ Api.app fileDbConfig) action


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
