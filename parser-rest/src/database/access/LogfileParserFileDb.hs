module LogfileParserFileDb
( readAll
, save
) where

import CustomParsers (LogfileParser)



readAll :: IO [LogfileParser]
readAll = do
  contents <- readFile "assets/logfile_parsers.txt"
  return (toParsers contents)


save :: LogfileParser -> IO ()
save logfileParser = do
  allParsers <- readAll
  putStr $ show allParsers
  writeFile "assets/logfile_parsers.txt" $ show (logfileParser:allParsers)


toParsers :: String -> [LogfileParser]
toParsers contents = read contents
