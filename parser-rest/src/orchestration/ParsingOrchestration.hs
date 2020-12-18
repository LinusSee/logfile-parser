module ParsingOrchestration
( existingLogfileParserNames
, createLogfileParser
, existingElementaryParsers
, createElementaryParser
, applyElementaryParser
, applyElementaryParserByName
, applyLogfileParser
, applyLogfileParserByName
) where

import CustomParsers
  ( ElementaryParser (..)
  , ParsingResult (..)
  , ParsingRequest (..)
  , ParsingResponse (..)
  , LogfileParser (..)
  , LogfileParsingRequest (..)
  , LogfileParsingResponse (..)
  )
import qualified ElementaryParsing as ElementaryParsing
import qualified ElementaryParserFileDb as ElemFileDb
import qualified LogfileParsing as LogfileParsing
import qualified LogfileParserFileDb as LogFileDb



existingLogfileParserNames :: IO [String]
existingLogfileParserNames = do
  parsers <- LogFileDb.readAll

  return $ map extractName parsers

  where extractName ( LogfileParser name _ ) = name


createLogfileParser :: LogfileParser -> IO ()
createLogfileParser logfileParser = LogFileDb.save logfileParser


existingElementaryParsers :: IO [ElementaryParser]
existingElementaryParsers = ElemFileDb.readAll


createElementaryParser :: ElementaryParser -> IO ()
createElementaryParser elementaryParser = ElemFileDb.save elementaryParser


applyElementaryParser :: ParsingRequest -> ParsingResponse
applyElementaryParser ( ParsingRequest target parser ) = do
  case parsingResult of
    Left err ->
      ParsingResponse "dummyError" (ParsingError (show err))

    Right result ->
      ParsingResponse "dummyName" result

    where parsingResult = ElementaryParsing.applyParser target parser


applyElementaryParserByName :: String -> String -> IO ParsingResponse
applyElementaryParserByName parserName target = do
  parsers <- ElemFileDb.readAll
  let parser = head $ filter byName parsers
  let parsingResult = ElementaryParsing.applyParser target parser

  case parsingResult of
    Left err ->
      return $ ParsingResponse "dummyError" (ParsingError (show err))

    Right result ->
      return $ ParsingResponse "dummyName" result

    where byName (OneOf name _ ) = name == parserName
          byName (Time name _ ) = name == parserName
          byName (Date name _ ) = name == parserName
          byName (Characters name _ ) = name == parserName


applyLogfileParser :: LogfileParsingRequest -> LogfileParsingResponse
applyLogfileParser ( LogfileParsingRequest target parser ) = do
  case parsingResult of
    Left err ->
      LogfileParsingError (show err)

    Right result ->
      result

  where parsingResult = LogfileParsing.applyLogfileParser target parser


applyLogfileParserByName :: String -> String -> IO LogfileParsingResponse
applyLogfileParserByName parserName target = do
  parsers <- LogFileDb.readAll
  let parser = head $ filter byName parsers
  let parsingResult = LogfileParsing.applyLogfileParser target parser

  case parsingResult of
    Left err ->
      return $ LogfileParsingError (show err)

    Right result ->
      return result

  where byName (LogfileParser name _ ) = name == parserName
