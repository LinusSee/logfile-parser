module ElementaryParserFileDb
  ( readAll
  , save
  ) where

import System.IO

import CustomParsers



readAll :: IO ([ElementaryParser])
readAll = do
  contents <- readFile "assets/parsers.txt"
  return (toParsers contents)


save :: ElementaryParser -> IO ()
save parser = do
  allParsers <- readAll
  putStr $ show allParsers
  writeFile "assets/parsers.txt" $ show (parser:allParsers)


toParsers :: String -> [ElementaryParser]
toParsers contents = read contents
