{-# LANGUAGE FlexibleContexts #-}


module LogfileParsing
( applyLogfileParser
) where

import qualified Text.Parsec as Parsec

import CustomParsers
  ( ElementaryParser (..)
  , LogfileParser (..)
  , ParsingResponse (..)
  , LogfileParsingResponse (..)
  )

import ElementaryParsing as ElementaryParsing



parse rule text = Parsec.parse rule "Logfile parser (source name)" text


applyLogfileParser :: String -> LogfileParser -> Either Parsec.ParseError LogfileParsingResponse
applyLogfileParser target (LogfileParser name parsers) =
  parse (applyListOfParsers parsers) target


applyListOfParsers :: [(String, ElementaryParser)] -> Parsec.Parsec String () LogfileParsingResponse
applyListOfParsers parsers = do
  result <- runThroughList parsers
  return $ LogfileParsingResponse result


runThroughList :: [ (String, ElementaryParser) ] -> Parsec.Parsec String () [ParsingResponse]
runThroughList [] = do
  return []
runThroughList ((resultName, parser):xs) = do
  result <- ElementaryParsing.chooseParser parser
  let namedResult = ParsingResponse resultName result
  next <- runThroughList xs
  return (namedResult : next)

  where extractName (OneOf name _ ) = name
        extractName (Time name _ ) = name
        extractName (Date name _ ) = name
        extractName (Characters name _ ) = name
