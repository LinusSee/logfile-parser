{-# LANGUAGE FlexibleContexts #-}

module LogfileParsing
( applyParser
, applyLogfileParser
) where

import qualified Text.Parsec as Parsec
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Time
import CustomParsers
  ( ElementaryParser (..)
  , LogfileParser (..)
  , ParsingRequest (..)
  , ParsingResponse (..)
  , LogfileParsingRequest (..)
  , LogfileParsingResponse (..)
  )


parse rule text = Parsec.parse rule "Logfile parser (source name)" text


applyParser :: String -> ElementaryParser -> Either Parsec.ParseError ParsingResponse
applyParser target parser =
  case parser of
    OneOf _ xs ->
      parse (applyOneOf xs) target

    Time _ pattern ->
      parse (applyTime pattern) target

    Date _ pattern ->
      parse (applyDate pattern) target

    Characters _ chars ->
      parse (applyCharacters chars) target


applyLogfileParser :: String -> LogfileParser -> Either Parsec.ParseError LogfileParsingResponse
applyLogfileParser target (LogfileParser name parsers) =
  parse (applyListOfParsers parsers) target


applyListOfParsers :: [ElementaryParser] -> Parsec.Parsec String () LogfileParsingResponse
applyListOfParsers parsers = do
  result <- Parsec.choice (map Parsec.string ["some", "values"])
  return $ LogfileParsingResponse result

applyOneOf :: [ String ] -> Parsec.Parsec String () ParsingResponse
applyOneOf target = do
  result <- Parsec.choice (map Parsec.string target)
  return $ OneOfResponse result


applyTime :: String -> Parsec.Parsec String () ParsingResponse
applyTime format = do
  result <- timePatternToParsers format
  -- "%H:%M:%S.%q" and replicate 9 '0'
  let time = parseTimeM False defaultTimeLocale "%H:%M" result :: Maybe TimeOfDay
  case time of
    Just parsedTime ->
      return $ TimeResponse (show parsedTime)

    Nothing ->
      return $ TimeResponse (result ++ "-asString")


applyDate :: String -> Parsec.Parsec String () ParsingResponse
applyDate format = do
  result <- datePatternToParsers format
  -- "%H:%M:%S.%q" and replicate 9 '0'
  let day = parseTimeM False defaultTimeLocale "%Y%m%d" result :: Maybe Day
  case day of
    Just parsedDay ->
      return $ DateResponse (show parsedDay)

    Nothing ->
      return $ DateResponse (result ++ "-asString")


  -- result <- Parsec.string "target"
  -- return $ DateResponse result


applyCharacters :: String -> Parsec.Parsec String () ParsingResponse
applyCharacters target = do
  result <- Parsec.string target
  return $ CharactersResponse result


datePatternToParsers :: String -> Parsec.Parsec String () String
datePatternToParsers pattern =
  case take 2 pattern of
    "MM" ->
      case take 2 $ drop 3 pattern of
        "DD" -> do
          month <- monthParser
          _ <- Parsec.char $ head (drop 2 pattern)
          day <- dayParser
          _ <- Parsec.char $ head (drop 5 pattern)
          year <- yearParser
          return (year ++ month ++ day)

        _ -> do
          month <- monthParser
          _ <- Parsec.char $ head (drop 2 pattern)
          year <- yearParser
          _ <- Parsec.char $ head (drop 7 pattern)
          day <- dayParser
          return (year ++ month ++ day)

    "DD" ->
      case take 2 $ drop 3 pattern of
        "MM" -> do
          day <- dayParser
          _ <- Parsec.char $ head (drop 2 pattern)
          month <- monthParser
          _ <- Parsec.char $ head (drop 5 pattern)
          year <- yearParser
          return (year ++ month ++ day)

        _ -> do
          day <- dayParser
          _ <- Parsec.char $ head (drop 2 pattern)
          year <- yearParser
          _ <- Parsec.char $ head (drop 7 pattern)
          month <- monthParser
          return (year ++ month ++ day)

    _ ->
      case take 2 $ drop 5 pattern of
        "MM" -> do
          year <- yearParser
          _ <- Parsec.char $ head (drop 4 pattern)
          month <- monthParser
          _ <- Parsec.char $ head (drop 7 pattern)
          day <- dayParser
          return (year ++ month ++ day)

        "DD" -> do
          year <- yearParser
          _ <- Parsec.char $ head (drop 4 pattern)
          day <- dayParser
          _ <- Parsec.char $ head (drop 7 pattern)
          month <- monthParser
          return (year ++ month ++ day)


timePatternToParsers :: String -> Parsec.Parsec String () String
timePatternToParsers pattern =
  let combineTime h m = h ++ (':' : m)
      separatorParser = Parsec.char $ head (drop 2 pattern)
  in

  case take 2 pattern of
    "HH" -> do
      hour <- hourParser
      _ <- Parsec.char $ head (drop 2 pattern)
      minute <- minuteParser
      return $ combineTime hour minute

    _ -> do
      minute <- minuteParser
      _ <- separatorParser
      hour <- hourParser
      return $ combineTime hour minute


hourParser :: Parsec.Parsec String () String
hourParser = Parsec.choice $ reverse
    (map (Parsec.try . Parsec.string . show) [2..24])
    ++ [Parsec.string "1"]
  --Parsec.choice $ reverse (map (Parsec.string . show) [1..24])


minuteParser :: Parsec.Parsec String () String
minuteParser = Parsec.choice $ reverse
    (map (Parsec.try . Parsec.string . show) [1..59])
    ++ [Parsec.string "1"]
  --Parsec.choice $ reverse (map (Parsec.string . show) [1..59])

yearParser :: Parsec.Parsec String () String
yearParser = do
  start <- Parsec.choice $ map (Parsec.string . show) [1..9]
  rest <- Parsec.count 3 (Parsec.choice $ map (Parsec.string . show) [0..9])
  return (start ++ intercalate "" rest)


monthParser :: Parsec.Parsec String () String
monthParser = do
  result <- Parsec.count 2 (Parsec.choice $ map (Parsec.string . show) [0..9])
  return $ intercalate "" result


dayParser :: Parsec.Parsec String () String
dayParser = do
  result <- Parsec.count 2 (Parsec.choice $ map (Parsec.string . show) [0..9])
  return $ intercalate "" result
