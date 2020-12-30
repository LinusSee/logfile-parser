module LogfileParserFileDb
( readAll
, save
) where

import qualified Data.Text as T

import CustomParsers (LogfileParser)
import qualified Configs as Configs



readAll :: Configs.FileDbConfig -> IO [LogfileParser]
readAll dbConfig = do
  contents <- readFile filePath

  return (toParsers contents)

  where filePath = T.unpack $ Configs.logfileParserPath dbConfig


save :: Configs.FileDbConfig -> LogfileParser -> IO ()
save dbConfig logfileParser = do
  allParsers <- readAll dbConfig
  putStr $ show allParsers
  writeFile filePath $ show (logfileParser:allParsers)

  where filePath = T.unpack $ Configs.logfileParserPath dbConfig


toParsers :: String -> [LogfileParser]
toParsers contents = read contents
