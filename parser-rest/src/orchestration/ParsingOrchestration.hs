module ParsingOrchestration
( applyElementaryParser
, applyElementaryParserByName
, applyLogfileParser
, applyLogfileParserByName
) where

import CustomParsers
  ( ElementaryParser (..)
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


-- TODO: Rename into stuff like -> processParsingRequest
-- TODO: Is it possible to get rid of IO? of course... just use let or where...
applyElementaryParser :: ParsingRequest -> IO ParsingResponse
applyElementaryParser ( ParsingRequest target parser ) = do
  let parsingResult = ElementaryParsing.applyParser target parser

  case parsingResult of
    Left err ->
      return $ ParsingError (show err)
      
    Right result ->
      return result


applyElementaryParserByName :: String -> String -> IO ParsingResponse
applyElementaryParserByName parserName target = do
  parsers <- ElemFileDb.readAll
  let parser = head $ filter byName parsers
  let parsingResult = ElementaryParsing.applyParser target parser

  case parsingResult of
    Left err ->
      return $ ParsingError (show err)

    Right result ->
      return result

    where byName (OneOf name _ ) = name == parserName
          byName (Time name _ ) = name == parserName
          byName (Date name _ ) = name == parserName
          byName (Characters name _ ) = name == parserName


applyLogfileParser :: LogfileParsingRequest -> IO LogfileParsingResponse
applyLogfileParser ( LogfileParsingRequest target parser ) = do
  let parsingResult = LogfileParsing.applyLogfileParser target parser

  case parsingResult of
    Left err ->
      return $ LogfileParsingError (show err)

    Right result ->
      return $ result


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
