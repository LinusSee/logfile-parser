module ElementaryParserFileDb
  ( readAll
  , save
  ) where

import qualified Data.Text as T

import CustomParsers ( ElementaryParser )
import qualified FileDbConfig as DbConfig



readAll :: DbConfig.FileDbConfig -> IO ([ElementaryParser])
readAll dbConfig = do
  contents <- readFile filePath

  return (toParsers contents)

  where filePath = T.unpack $ DbConfig.elementaryParserPath dbConfig


save :: DbConfig.FileDbConfig -> ElementaryParser -> IO ()
save dbConfig parser = do
  allParsers <- readAll dbConfig
  putStr $ show allParsers
  writeFile filePath $ show (parser:allParsers)

  where filePath = T.unpack $ DbConfig.elementaryParserPath dbConfig


toParsers :: String -> [ElementaryParser]
toParsers contents = read contents
