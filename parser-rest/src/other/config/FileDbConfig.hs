{-# LANGUAGE OverloadedStrings #-}

module FileDbConfig
( FileDbConfig (..)
, fileDbConfig
) where

import qualified Data.Text as T
import Data.Ini.Config



data FileDbConfig = FileDbConfig
  { elementaryParserPath :: T.Text
  , logfileParserPath :: T.Text
  } deriving (Show)

fileDbConfig :: T.Text -> Either String FileDbConfig
fileDbConfig configFile = parseIniFile configFile fileDbConfigParser

fileDbConfigParser :: IniParser FileDbConfig
fileDbConfigParser =
  section "FileDb" $ do
    elementaryPath <- fieldOf "elementaryParserFile" string
    logfilePath <- fieldOf "logfileParserFile" string

    return $ FileDbConfig elementaryPath logfilePath
