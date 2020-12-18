{-# LANGUAGE FlexibleContexts #-}


module LogfileParsing
( applyLogfileParser
) where

import qualified Text.Parsec as Parsec

import CustomParsers
  ( ElementaryParser
  , LogfileParser (..)
  , ParsingResponse (..)
  , LogfileParsingResponse (..)
  )

import ElementaryParsing as ElementaryParsing



parse rule text = Parsec.parse rule "Logfile parser (source name)" text


applyLogfileParser :: String -> LogfileParser -> Either Parsec.ParseError LogfileParsingResponse
applyLogfileParser target (LogfileParser name parsers) =
  parse (applyListOfParsers parsers) target


applyListOfParsers :: [ElementaryParser] -> Parsec.Parsec String () LogfileParsingResponse
applyListOfParsers parsers = do
  result <- runThroughList parsers
  return $ LogfileParsingResponse result


runThroughList :: [ ElementaryParser ] -> Parsec.Parsec String () [ParsingResponse]
runThroughList [] = do
  return []
runThroughList (x:xs) = do
  result <- ElementaryParsing.chooseParser x
  let namedResult = ParsingResponse "dummyName" result
  next <- runThroughList xs
  return (namedResult : next)
