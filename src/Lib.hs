module Lib
    ( someFunc
    , dateParser
    , dateTimeParser
    , loglevelParser
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

timeParser :: Parsec.Parsec String () String
timeParser = do
  hours <- Parsec.count 2 Parsec.digit
  sep1 <- Parsec.char ':'
  minutes <- Parsec.count 2 Parsec.digit
  sep2 <- Parsec.char ':'
  seconds <- Parsec.count 2 Parsec.digit
  sep3 <- Parsec.char ','
  ms <- Parsec.count 3 Parsec.digit
  return (hours ++ [sep1] ++ minutes ++ [sep2] ++ seconds ++ [sep3] ++ ms)

dateTimeParser :: Parsec.Parsec String () String
dateTimeParser = do
  date <- dateParser
  Parsec.char ' '
  time <- timeParser
  return (date ++ [' '] ++ time)

loglevelParser :: Parsec.Parsec String () String
loglevelParser = do
  Parsec.choice (map Parsec.string ["DEBUG", "WARN"])

--toParseThemAll :: Parsec.Parsec String () (String, String)
--toParseThemAll = do



someFunc :: IO ()
someFunc = putStrLn "someFunc"
