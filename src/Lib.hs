module Lib
    ( someFunc
    , dateParser
    ) where

import qualified Text.Parsec as Parsec

dateParser :: Parsec.Parsec String () String
dateParser = do
  year <- Parsec.count 4 Parsec.digit
  sep1 <- Parsec.char '-'
  month <- Parsec.count 2 Parsec.digit
  sep2 <- Parsec.char '-'
  day <- Parsec.count 2 Parsec.digit
  return (year ++ [sep1] ++ month ++ [sep2] ++ day)

--toParseThemAll :: Parsec.Parsec String () (String, String)
--toParseThemAll = do



someFunc :: IO ()
someFunc = putStrLn "someFunc"
