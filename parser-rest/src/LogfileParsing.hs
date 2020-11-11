{-# LANGUAGE FlexibleContexts #-}

module LogfileParsing
( applyParser
) where

import qualified Text.Parsec as Parsec
import CustomParsers (ElementaryParser(..), ParsingRequest(..))


parse rule text = Parsec.parse rule "Logfile parser (source name)" text


applyParser :: String -> ElementaryParser -> Either Parsec.ParseError String
applyParser target parser =
  case parser of
    OneOf _ xs ->
      parse applyOneOf target -- TODO: Incorrect
    Time _ pattern ->
      parse applyTime target -- TODO: Incorrect
    Date _ pattern ->
      parse applyDate target -- TODO: Incorrect
    Characters _ chars ->
      parse (applyCharacters chars) target

applyOneOf :: Parsec.Parsec String () String
applyOneOf = do
  result <- Parsec.string "target"
  return result

applyTime :: Parsec.Parsec String () String
applyTime = do
  result <- Parsec.string "target"
  return result

applyDate :: Parsec.Parsec String () String
applyDate = do
  result <- Parsec.string "target"
  return result

applyCharacters :: String -> Parsec.Parsec String () String
applyCharacters target = do
  result <- Parsec.string target
  return result
