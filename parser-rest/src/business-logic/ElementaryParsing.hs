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
  , BasicParser (..)
  , ParsingResult (..)
  )
import qualified BusinessLogicModels as BM


parse rule text = Parsec.parse rule "Logfile parser (source name)" text


applyParser :: String -> BM.ParserType -> Either Parsec.ParseError BM.ParsingResultType
applyParser target parser =
    parse chosenParser target

    where chosenParser = chooseParser parser


chooseParser :: BM.ParserType -> Parsec.Parsec String () BM.ParsingResultType
chooseParser basicParser =
  case basicParser of
    BM.OneOf xs ->
      applyOneOf xs

    BM.Time pattern ->
      applyTime pattern

    BM.Date pattern ->
      applyDate pattern

    BM.Characters chars ->
      applyCharacters chars

    BM.MatchUntilIncluded chars ->
      applyMatchUntilIncluded chars

    BM.MatchUntilExcluded chars ->
      applyMatchUntilExcluded chars

    BM.MatchFor count ->
      applyMatchFor count

    BM.MatchUntilEnd ->
      applyMatchUntilEnd





applyOneOf :: [ String ] -> Parsec.Parsec String () BM.ParsingResultType
applyOneOf target = do
  result <- Parsec.choice (map (Parsec.try .Parsec.string) target)
  return $ BM.OneOfResult result


applyTime :: String -> Parsec.Parsec String () BM.ParsingResultType
applyTime format = do
  result <- timePatternToParsers (map toUpper format)
  -- "%H:%M:%S.%q" and replicate 9 '0'
  let time = parseTimeM False defaultTimeLocale "%H:%M" result :: Maybe TimeOfDay
  case time of
    Just parsedTime ->
      return $ BM.TimeResult parsedTime

    Nothing ->
      return $ BM.ParsingError ("Could not convert '" ++ result ++ "' to a time'")


applyDate :: String -> Parsec.Parsec String () BM.ParsingResultType
applyDate format = do
  result <- datePatternToParsers (map toUpper format)
  -- "%H:%M:%S.%q" and replicate 9 '0'
  let day = parseTimeM False defaultTimeLocale "%Y%m%d" result :: Maybe Day
  case day of
    Just parsedDay ->
      return $ BM.DateResult parsedDay

    Nothing ->
      return $ BM.ParsingError ("Could not convert '" ++ result ++ "' to a date'")


applyCharacters :: String -> Parsec.Parsec String () BM.ParsingResultType
applyCharacters target = do
  result <- Parsec.string target
  return $ BM.CharactersResult result


applyMatchUntilIncluded :: String -> Parsec.Parsec String () BM.ParsingResultType
applyMatchUntilIncluded value = do
  untilIncluded <- Parsec.manyTill Parsec.anyChar (Parsec.lookAhead (Parsec.try $ Parsec.string value))
  included <- Parsec.string value
  let result = untilIncluded ++ included

  return $ BM.MatchUntilIncludedResult result


applyMatchUntilExcluded :: String -> Parsec.Parsec String () BM.ParsingResultType
applyMatchUntilExcluded value = do
  result <- Parsec.manyTill Parsec.anyChar (Parsec.lookAhead (Parsec.try $ Parsec.string value))

  return $ BM.MatchUntilExcludedResult result


applyMatchFor :: Int -> Parsec.Parsec String () BM.ParsingResultType
applyMatchFor count = do
  result <- Parsec.count count Parsec.anyChar

  return $ BM.MatchForResult result


applyMatchUntilEnd :: Parsec.Parsec String () BM.ParsingResultType
applyMatchUntilEnd = do
  result <- Parsec.manyTill Parsec.anyChar (Parsec.lookAhead eolParser)

  return $ BM.MatchUntilEndResult result


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
          let sep1 = head (drop 2 pattern)
          _ <- Parsec.choice [Parsec.char sep1, Parsec.char $ toLower sep1]
          day <- dayParser
          let sep2 = head (drop 5 pattern)
          _ <- Parsec.choice [ Parsec.char sep2, Parsec.char $ toLower sep2]
          year <- yearParser
          return (year ++ month ++ day)

        _ -> do
          month <- monthParser
          let sep1 = head (drop 2 pattern)
          _ <- Parsec.choice [Parsec.char sep1, Parsec.char $ toLower sep1]
          year <- yearParser
          let sep2 = head (drop 7 pattern)
          _ <- Parsec.choice [Parsec.char sep2, Parsec.char $ toLower sep2]
          day <- dayParser
          return (year ++ month ++ day)

    "DD" ->
      case take 2 $ drop 3 pattern of
        "MM" -> do
          day <- dayParser
          let sep1 = head (drop 2 pattern)
          _ <- Parsec.choice [Parsec.char sep1, Parsec.char $ toLower sep1]
          month <- monthParser
          let sep2 = head (drop 5 pattern)
          _ <- Parsec.choice [Parsec.char sep2, Parsec.char $ toLower sep2]
          year <- yearParser
          return (year ++ month ++ day)

        _ -> do
          day <- dayParser
          let sep1 = head (drop 2 pattern)
          _ <- Parsec.choice [Parsec.char sep1, Parsec.char $ toLower sep1]
          year <- yearParser
          let sep2 = head (drop 7 pattern)
          _ <- Parsec.choice [Parsec.char sep2, Parsec.char $ toLower sep2]
          month <- monthParser
          return (year ++ month ++ day)

    _ ->
      case take 2 $ drop 5 pattern of
        "MM" -> do
          year <- yearParser
          let sep1 = head (drop 4 pattern)
          _ <- Parsec.choice [Parsec.char sep1, Parsec.char $ toLower sep1]
          month <- monthParser
          let sep2 = head (drop 7 pattern)
          _ <- Parsec.choice [Parsec.char sep2, Parsec.char $ toLower sep2]
          day <- dayParser
          return (year ++ month ++ day)

        "DD" -> do
          year <- yearParser
          let sep1 = head (drop 4 pattern)
          _ <- Parsec.choice [Parsec.char sep1, Parsec.char $ toLower sep1]
          day <- dayParser
          let sep2 = head (drop 7 pattern)
          _ <- Parsec.choice [Parsec.char sep2, Parsec.char $ toLower sep2]
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
hourParser = fmap normalize (Parsec.choice $ map (Parsec.try . Parsec.string) hourPossibilities)

    where hourPossibilities = map (('0':) . show) [0..9] ++ map show [10..23] ++ map show [0..9]
          normalize hour =
              if length hour < 2
              then '0': hour
              else hour


minuteParser :: Parsec.Parsec String () String
minuteParser = Parsec.choice $ map (Parsec.try . Parsec.string) minutePossibilities

    where minutePossibilities = map (('0':) . show) [0..9] ++ map show [10..59]
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
