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
  putStr $ show allParsers
  writeFile "assets/parsers.txt" $ show (parser:allParsers)


sampleParsers :: [ElementaryParser]
sampleParsers =
  [ OneOf "oneOfWorld" ["Hello", "World", "!"]
  , Date "easySortDate" "yyyy-mm-dd"
  , Date "weirdDate" "yyyy-dd-mm"
  , Date "clockTime" "HH:mm"
  , Characters "RandomString" "Some string to match"
  ]

toParsers :: String -> [ElementaryParser]
toParsers contents = read contents
