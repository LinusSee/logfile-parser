{-# LANGUAGE OverloadedStrings #-}

module Configs
( Config (..)
, FileDbConfig (..)
, readFileDbConfig
, APIConfig (..)
, readApiConfig
) where

import qualified Data.Text as T
import Data.Ini.Config


data Config = Config
  { fileDbConfig :: FileDbConfig
  , apiConfig :: APIConfig
  }



data FileDbConfig = FileDbConfig
  { elementaryParserPath :: T.Text
  , logfileParserPath :: T.Text
  } deriving (Show)

readFileDbConfig :: T.Text -> Either String FileDbConfig
readFileDbConfig configFile = parseIniFile configFile fileDbConfigParser

fileDbConfigParser :: IniParser FileDbConfig
fileDbConfigParser =
  section "FileDb" $ do
    elementaryPath <- fieldOf "elementaryParserFile" string
    logfilePath <- fieldOf "logfileParserFile" string

    return $ FileDbConfig elementaryPath logfilePath



data APIConfig = APIConfig
  { port :: Int
  }


readApiConfig :: T.Text -> Either String APIConfig
readApiConfig configFile = parseIniFile configFile apiConfigParser

apiConfigParser :: IniParser APIConfig
apiConfigParser =
  section "API" $ do
    port <- fieldOf "port" number

    return $ APIConfig port
