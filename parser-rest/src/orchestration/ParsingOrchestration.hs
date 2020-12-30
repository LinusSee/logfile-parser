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
import qualified Configs as Configs
import qualified ElementaryParsing as ElementaryParsing
import qualified ElementaryParserFileDb as ElemFileDb
import qualified LogfileParsing as LogfileParsing
import qualified LogfileParserFileDb as LogFileDb



existingLogfileParserNames :: Configs.FileDbConfig -> IO [String]
existingLogfileParserNames dbConfig = do
  parsers <- LogFileDb.readAll dbConfig

  return $ map extractName parsers

  where extractName ( LogfileParser name _ ) = name


createLogfileParser :: Configs.FileDbConfig -> CreateLogfileParserRequest -> IO ()
createLogfileParser dbConfig (CreateLogfileParserRequest name parsers) =
  LogFileDb.save dbConfig logfileParser
  where logfileParser = LogfileParser name mappedParsers
        mapParser ( NamedParser name parser ) = (name, parser)
        mappedParsers = map mapParser parsers


existingElementaryParsers :: Configs.FileDbConfig -> IO [ElementaryParser]
existingElementaryParsers dbConfig =
  ElemFileDb.readAll dbConfig


createElementaryParser :: Configs.FileDbConfig -> ElementaryParser -> IO ()
createElementaryParser dbConfig elementaryParser =
  ElemFileDb.save dbConfig elementaryParser


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
          extractName (MatchUntilIncluded name _ ) = name
          extractName (MatchUntilExcluded name _ ) = name
          extractName (MatchFor name _ ) = name
          extractName (MatchUntilEnd name ) = name


applyElementaryParserByName :: Configs.FileDbConfig -> String -> String -> IO ParsingResponse
applyElementaryParserByName dbConfig parserName target = do
  parsers <- ElemFileDb.readAll dbConfig
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
          byName (MatchUntilIncluded name _ ) = name == parserName
          byName (MatchUntilExcluded name _ ) = name == parserName
          byName (MatchFor name _ ) = name == parserName
          byName (MatchUntilEnd name ) = name == parserName


applyLogfileParser :: LogfileParsingRequest -> LogfileParsingResponse
applyLogfileParser ( LogfileParsingRequest target (CreateLogfileParserRequest name parsers) ) = do
  case parsingResult of
    Left err ->
      LogfileParsingError (show err)

    Right result ->
      result

  where logfileParser = LogfileParser name mappedParsers
        mapParser ( NamedParser name parser ) = (name, parser)
        mappedParsers = map mapParser parsers
        parsingResult = LogfileParsing.applyLogfileParser target logfileParser


applyLogfileParserByName :: Configs.FileDbConfig -> String -> String -> IO LogfileParsingResponse
applyLogfileParserByName dbConfig parserName target = do
  parsers <- LogFileDb.readAll dbConfig
  let parser = head $ filter byName parsers
  let parsingResult = LogfileParsing.applyLogfileParser target parser

  case parsingResult of
    Left err ->
      return $ LogfileParsingError (show err)

    Right result ->
      return result

  where byName (LogfileParser name _ ) = name == parserName
