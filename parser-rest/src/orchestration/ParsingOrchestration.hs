module ParsingOrchestration
( existingLogfileParserNames
, createLogfileParser
, existingElementaryParsers
, createElementaryParser
, applyElementaryParser
, applyElementaryParserByName
, applyLogfileParser
, applyLogfileParserToFile
, applyLogfileParserByName
) where

import CustomParsers
  ( ElementaryParser (..)
  , BasicParser (..)
  , ParsingResult (..)
  , NamedParsingResult (..)
  , ParsingRequest (..)
  , ParsingResponse (..)
  , LogfileParser (..)
  , LogfileParsingResult (..)
  , LogfileParsingRequest (..)
  , LogfileParsingFileRequest (..)
  , LogfileParsingResponse (..)
  , CreateLogfileParserRequest (..)
  , NamedElementaryParser (..)
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

  where logfileParser = LogfileParser name parsers


existingElementaryParsers :: Configs.FileDbConfig -> IO [ElementaryParser]
existingElementaryParsers dbConfig =
  ElemFileDb.readAll dbConfig


createElementaryParser :: Configs.FileDbConfig -> ElementaryParser -> IO ()
createElementaryParser dbConfig elementaryParser =
  ElemFileDb.save dbConfig elementaryParser


applyElementaryParser :: ParsingRequest -> ParsingResponse
applyElementaryParser ( ParsingRequest target (ElementaryParser name parser) ) = do
  case parsingResult of
    Left err ->
      ParsingResponse name (ParsingError (show err))

    Right result ->
      ParsingResponse name result

    where parsingResult = ElementaryParsing.applyParser target parser


applyElementaryParserByName :: Configs.FileDbConfig -> String -> String -> IO ParsingResponse
applyElementaryParserByName dbConfig parserName target = do
  parsers <- ElemFileDb.readAll dbConfig
  let (ElementaryParser _ parser) = head $ filter byName parsers
  let parsingResult = ElementaryParsing.applyParser target parser

  case parsingResult of
    Left err ->
      return $ ParsingResponse parserName (ParsingError (show err))

    Right result ->
      return $ ParsingResponse parserName result

    where byName (ElementaryParser name _) = name == parserName


applyLogfileParser :: LogfileParsingRequest -> LogfileParsingResponse
applyLogfileParser ( LogfileParsingRequest target (CreateLogfileParserRequest name parsers) ) = do
  case parsingResult of
    Left err ->
      LogfileParsingError (show err)

    Right result ->
      toLogfileParsingResponse result

  where logfileParser = LogfileParser name parsers
        parsingResult = LogfileParsing.applyLogfileParser target logfileParser


applyLogfileParserToFile :: Configs.FileDbConfig -> LogfileParsingFileRequest -> IO LogfileParsingResponse
applyLogfileParserToFile dbConfig (LogfileParsingFileRequest parserName logfile) = do
  parsers <- LogFileDb.readAll dbConfig
  target <- readFile "./assets/sample_log4j.log"

  let parser = head $ filter byName parsers
  let parsingResult = LogfileParsing.applyLogfileParser target parser

  case parsingResult of
    Left err ->
      return $ LogfileParsingError (show err)

    Right result ->
      return $ toLogfileParsingResponse result

  where byName (LogfileParser name _ ) = name == parserName


applyLogfileParserByName :: Configs.FileDbConfig -> String -> String -> IO LogfileParsingResponse
applyLogfileParserByName dbConfig parserName target = do
  parsers <- LogFileDb.readAll dbConfig
  let parser = head $ filter byName parsers
  let parsingResult = LogfileParsing.applyLogfileParser target parser

  case parsingResult of
    Left err ->
      return $ LogfileParsingError (show err)

    Right result ->
      return $ toLogfileParsingResponse result

  where byName (LogfileParser name _ ) = name == parserName


toLogfileParsingResponse :: LogfileParsingResult -> LogfileParsingResponse
toLogfileParsingResponse (LogfileParsingSuccess result) =
    let toParsingResult (NamedParsingResult n r) = ParsingResponse n r
    in  LogfileParsingResponse $ map (map toParsingResult) result
toLogfileParsingResponse (LogfileParsingFailure err) =
    LogfileParsingError err
