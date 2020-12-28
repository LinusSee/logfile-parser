module LogfileParserFileDb
( readAll
, save
) where

import qualified Data.Text as T

import CustomParsers (LogfileParser)
import qualified FileDbConfig as DbConfig



readAll :: DbConfig.FileDbConfig -> IO [LogfileParser]
readAll dbConfig = do
  contents <- readFile filePath

  return (toParsers contents)

  where filePath = T.unpack $ DbConfig.logfileParserPath dbConfig


save :: DbConfig.FileDbConfig -> LogfileParser -> IO ()
save dbConfig logfileParser = do
  allParsers <- readAll dbConfig
  putStr $ show allParsers
  writeFile filePath $ show (logfileParser:allParsers)

  where filePath = T.unpack $ DbConfig.logfileParserPath dbConfig


toParsers :: String -> [LogfileParser]
toParsers contents = read contents
