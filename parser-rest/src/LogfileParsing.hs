{-# LANGUAGE FlexibleContexts #-}

module LogfileParsing
( applyParser
) where

import qualified Text.Parsec as Parsec
import CustomParsers (ElementaryParser(..), ParsingRequest(..), ParsingResponse(..))


parse rule text = Parsec.parse rule "Logfile parser (source name)" text


applyParser :: String -> ElementaryParser -> Either Parsec.ParseError ParsingResponse
applyParser target parser =
  case parser of
    OneOf _ xs ->
      parse (applyOneOf xs) target -- TODO: Incorrect
    Time _ pattern ->
      parse applyTime target -- TODO: Incorrect
    Date _ pattern ->
      parse applyDate target -- TODO: Incorrect
    Characters _ chars ->
      parse (applyCharacters chars) target

applyOneOf :: [ String ] -> Parsec.Parsec String () ParsingResponse
applyOneOf target = do
  result <- Parsec.choice (map Parsec.string target)
  return $ OneOfResponse result

applyTime :: Parsec.Parsec String () ParsingResponse
applyTime = do
  result <- Parsec.string "target"
  return $ TimeResponse result

applyDate :: Parsec.Parsec String () ParsingResponse
applyDate = do
  result <- Parsec.string "target"
  return $ DateResponse result

applyCharacters :: String -> Parsec.Parsec String () ParsingResponse
applyCharacters target = do
  result <- Parsec.string target
  return $ CharactersResponse result
