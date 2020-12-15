module ParsingOrchestration
( applyLogfileParser
, applyLogfileParserByName
) where

import CustomParsers
  ( LogfileParser (..)
  , LogfileParsingRequest (..)
  , LogfileParsingResponse (..)
  )
import qualified LogfileParsing as LogfileParsing
import qualified LogfileParserFileDb as LogFileDb


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
