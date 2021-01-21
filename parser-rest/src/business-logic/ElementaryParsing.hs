{-# LANGUAGE FlexibleContexts #-}


module ElementaryParsing
( applyParser
, chooseParser
) where

import qualified Text.Parsec as Parsec
import Data.Char
import Data.List
import Data.Time
import CustomParsers
  ( ElementaryParser (..)
  , ParsingResult (..)
  )



parse rule text = Parsec.parse rule "Logfile parser (source name)" text


applyParser :: String -> ElementaryParser -> Either Parsec.ParseError ParsingResult
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

    MatchUntilIncluded _ chars ->
      parse (applyMatchUntilIncluded chars) target

    MatchUntilExcluded _ chars ->
      parse (applyMatchUntilExcluded chars) target

    MatchFor _ count ->
      parse (applyMatchFor count) target

    MatchUntilEnd _ ->
      parse applyMatchUntilEnd target


chooseParser :: ElementaryParser -> Parsec.Parsec String () ParsingResult
chooseParser parser =
  case parser of
    OneOf _ xs ->
      applyOneOf xs

    Time _ pattern ->
      applyTime pattern

    Date _ pattern ->
      applyDate pattern

    Characters _ chars ->
      applyCharacters chars

    MatchUntilIncluded _ chars ->
      applyMatchUntilIncluded chars

    MatchUntilExcluded _ chars ->
      applyMatchUntilExcluded chars

    MatchFor _ count ->
      applyMatchFor count

    MatchUntilEnd _ ->
      applyMatchUntilEnd





applyOneOf :: [ String ] -> Parsec.Parsec String () ParsingResult
applyOneOf target = do
  result <- Parsec.choice (map (Parsec.try .Parsec.string) target)
  return $ OneOfResult result


applyTime :: String -> Parsec.Parsec String () ParsingResult
applyTime format = do
  result <- timePatternToParsers (map toUpper format)
  -- "%H:%M:%S.%q" and replicate 9 '0'
  let time = parseTimeM False defaultTimeLocale "%H:%M" result :: Maybe TimeOfDay
  case time of
    Just parsedTime ->
      return $ TimeResult (show parsedTime)

    Nothing ->
      return $ TimeResult (result ++ "-asString")


applyDate :: String -> Parsec.Parsec String () ParsingResult
applyDate format = do
  result <- datePatternToParsers (map toUpper format)
  -- "%H:%M:%S.%q" and replicate 9 '0'
  let day = parseTimeM False defaultTimeLocale "%Y%m%d" result :: Maybe Day
  case day of
    Just parsedDay ->
      return $ DateResult (show parsedDay)

    Nothing ->
      return $ DateResult (result ++ "-asString")


applyCharacters :: String -> Parsec.Parsec String () ParsingResult
applyCharacters target = do
  result <- Parsec.string target
  return $ CharactersResult result


applyMatchUntilIncluded :: String -> Parsec.Parsec String () ParsingResult
applyMatchUntilIncluded value = do
  untilIncluded <- Parsec.manyTill Parsec.anyChar (Parsec.lookAhead (Parsec.string value))
  included <- Parsec.string value
  let result = untilIncluded ++ included

  return $ MatchUntilIncludedResult result


applyMatchUntilExcluded :: String -> Parsec.Parsec String () ParsingResult
applyMatchUntilExcluded value = do
  result <- Parsec.manyTill Parsec.anyChar (Parsec.lookAhead (Parsec.string value))

  return $ MatchUntilExcludedResult result


applyMatchFor :: Int -> Parsec.Parsec String () ParsingResult
applyMatchFor count = do
  result <- Parsec.count count Parsec.anyChar

  return $ MatchForResult result


applyMatchUntilEnd :: Parsec.Parsec String () ParsingResult
applyMatchUntilEnd = do
  result <- Parsec.manyTill Parsec.anyChar (Parsec.lookAhead eolParser)

  return $ MatchUntilEndResult result


newlineParser :: Parsec.Parsec String () ()
newlineParser = do
  Parsec.choice [Parsec.string "\n", Parsec.string "\r\n"]
  return ()


eolParser :: Parsec.Parsec String () ()
eolParser = do
  Parsec.choice [newlineParser, Parsec.eof]
  return ()


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
