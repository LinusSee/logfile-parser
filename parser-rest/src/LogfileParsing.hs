{-# LANGUAGE FlexibleContexts #-}

module LogfileParsing
( applyParser
) where

import qualified Text.Parsec as Parsec
import Control.Monad.IO.Class (liftIO)
import Data.Time
import CustomParsers (ElementaryParser(..), ParsingRequest(..), ParsingResponse(..))


parse rule text = Parsec.parse rule "Logfile parser (source name)" text


applyParser :: String -> ElementaryParser -> Either Parsec.ParseError ParsingResponse
applyParser target parser =
  case parser of
    OneOf _ xs ->
      parse (applyOneOf xs) target

    Time _ pattern ->
      parse (applyTime pattern) target

    Date _ pattern ->
      parse applyDate target -- TODO: Incorrect

    Characters _ chars ->
      parse (applyCharacters chars) target


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


applyDate :: Parsec.Parsec String () ParsingResponse
applyDate = do
  result <- Parsec.string "target"
  return $ DateResponse result


applyCharacters :: String -> Parsec.Parsec String () ParsingResponse
applyCharacters target = do
  result <- Parsec.string target
  return $ CharactersResponse result


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
