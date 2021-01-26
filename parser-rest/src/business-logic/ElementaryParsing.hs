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
    parse chosenParser target

    where chosenParser = chooseParser parser


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
  untilIncluded <- Parsec.manyTill Parsec.anyChar (Parsec.lookAhead (Parsec.try $ Parsec.string value))
  included <- Parsec.string value
  let result = untilIncluded ++ included

  return $ MatchUntilIncludedResult result


applyMatchUntilExcluded :: String -> Parsec.Parsec String () ParsingResult
applyMatchUntilExcluded value = do
  result <- Parsec.manyTill Parsec.anyChar (Parsec.lookAhead (Parsec.try $ Parsec.string value))

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
