module FileDb
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
  writeFile "assets/parsers.txt" $ show (parser:allParsers)


sampleParsers :: [ElementaryParser]
sampleParsers =
  [ OneOf ["Hello", "World", "!"]
  , Date "yyyy-mm-dd"
  , Date "yyyy-dd-mm"
  , Date "HH:mm"
  , Characters "Some string to match"
  ]

toParsers :: String -> [ElementaryParser]
toParsers contents = read contents
