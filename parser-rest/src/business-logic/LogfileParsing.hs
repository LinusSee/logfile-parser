{-# LANGUAGE FlexibleContexts #-}


module LogfileParsing
( applyLogfileParser
) where

import qualified Text.Parsec as Parsec

import CustomParsers
  ( ElementaryParser (..)
  , NamedElementaryParser (..)
  , LogfileParser (..)
  , NamedParsingResult (..)
  , LogfileParsingResult (..)
  , ParsingOptions (..)
  , ParsingOption (..)
  )

import ElementaryParsing as ElementaryParsing



parse rule text = Parsec.parse rule "Logfile parser (source name)" text


applyLogfileParser :: String -> LogfileParser -> Either Parsec.ParseError LogfileParsingResult
applyLogfileParser target (LogfileParser _ parsers) =
  parse (fileParser parsers) target


fileParser :: [NamedElementaryParser] -> Parsec.Parsec String () LogfileParsingResult
fileParser parsers = do
  result <- Parsec.manyTill (applyListOfParsers parsers) Parsec.eof

  return $ LogfileParsingSuccess result
  -- TODO: If a single parser returns an error -> LogfileParsingFailure
  -- Can this even happen or is the error throw to the orchestration?


newlineParser :: Parsec.Parsec String () ()
newlineParser = do
  Parsec.choice [Parsec.string "\n", Parsec.string "\r\n"]
  return ()


eolParser :: Parsec.Parsec String () ()
eolParser = do
  Parsec.choice [newlineParser, Parsec.eof]
  return ()


applyListOfParsers :: [NamedElementaryParser] -> Parsec.Parsec String () [NamedParsingResult]
applyListOfParsers parsers = do
  result <- runThroughList parsers
  _ <- eolParser

  return result


runThroughList :: [NamedElementaryParser] -> Parsec.Parsec String () [NamedParsingResult]
runThroughList [] = do
  return []
runThroughList (x:xs) = do
  result <- ElementaryParsing.chooseParser basicParser
  let namedResult = NamedParsingResult resultName result
  next <- runThroughList xs
  return $ if keepResult options then (namedResult : next)
                                 else next

  where NamedElementaryParser resultName (ElementaryParser name (ParsingOptions options) basicParser) = x
        keepResult [] = False
        keepResult ((KeepResult x):xs) = x
        keepResult (x:xs) = keepResult xs
