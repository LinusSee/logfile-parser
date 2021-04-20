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

import qualified BusinessLogicModels as BM
import ElementaryParsing as ElementaryParsing



parse rule text = Parsec.parse rule "Logfile parser (source name)" text


applyLogfileParser :: String -> BM.LogfileParser -> Either Parsec.ParseError BM.LogfileParsingResult
applyLogfileParser target (BM.LogfileParser _ parsers) =
  parse (fileParser parsers) target


fileParser :: [BM.NamedElementaryParser] -> Parsec.Parsec String () BM.LogfileParsingResult
fileParser parsers = do
  result <- Parsec.manyTill (applyListOfParsers parsers) Parsec.eof

  return $ BM.LogfileParsingResult result
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


applyListOfParsers :: [BM.NamedElementaryParser] -> Parsec.Parsec String () [BM.ElementaryParsingResult]
applyListOfParsers parsers = do
  result <- runThroughList parsers
  _ <- eolParser

  return result


runThroughList :: [BM.NamedElementaryParser] -> Parsec.Parsec String () [BM.ElementaryParsingResult]
runThroughList [] = do
  return []
runThroughList (x:xs) = do
  result <- ElementaryParsing.chooseParser basicParser
  let namedResult = BM.ElementaryParsingResult resultName result
  next <- runThroughList xs
  return $ if BM.keepResult options then (namedResult : next)
                                    else next

  where BM.NamedElementaryParser resultName (BM.ElementaryParser name options basicParser) = x
