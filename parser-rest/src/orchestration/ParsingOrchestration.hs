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
  , CreateLogfileParserRequest (..)
  , NamedParser (..)
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


createLogfileParser :: CreateLogfileParserRequest -> IO ()
createLogfileParser (CreateLogfileParserRequest name parsers) = LogFileDb.save logfileParser
  where logfileParser = LogfileParser name mappedParsers
        mapParser ( NamedParser name parser ) = (name, parser)
        mappedParsers = map mapParser parsers


existingElementaryParsers :: IO [ElementaryParser]
existingElementaryParsers = ElemFileDb.readAll


createElementaryParser :: ElementaryParser -> IO ()
createElementaryParser elementaryParser = ElemFileDb.save elementaryParser


applyElementaryParser :: ParsingRequest -> ParsingResponse
applyElementaryParser ( ParsingRequest target parser ) = do
  case parsingResult of
    Left err ->
      ParsingResponse (extractName parser) (ParsingError (show err))

    Right result ->
      ParsingResponse (extractName parser) result

    where parsingResult = ElementaryParsing.applyParser target parser
          extractName (OneOf name _ ) = name
          extractName (Time name _ ) = name
          extractName (Date name _ ) = name
          extractName (Characters name _ ) = name


applyElementaryParserByName :: String -> String -> IO ParsingResponse
applyElementaryParserByName parserName target = do
  parsers <- ElemFileDb.readAll
  let parser = head $ filter byName parsers
  let parsingResult = ElementaryParsing.applyParser target parser

  case parsingResult of
    Left err ->
      return $ ParsingResponse parserName (ParsingError (show err))

    Right result ->
      return $ ParsingResponse parserName result

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
