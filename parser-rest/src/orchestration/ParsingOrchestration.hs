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
  , fromDbElementaryParser
  , toDbElementaryParser
  , fromDbLogfileParser
  , toDbLogfileParser
  )
import qualified Configs as Configs
import qualified ElementaryParsing as ElementaryParsing
import qualified ElementaryParserFileDb as ElemFileDb
import qualified LogfileParsing as LogfileParsing
import qualified LogfileParserFileDb as LogFileDb

import qualified BusinessLogicModels as BM



existingLogfileParserNames :: Configs.FileDbConfig -> IO [String]
existingLogfileParserNames dbConfig = do
  parsers <- LogFileDb.readAll dbConfig

  return $ map extractName (map fromDbLogfileParser parsers)

  where extractName ( BM.LogfileParser name _ ) = name


createLogfileParser :: Configs.FileDbConfig -> BM.LogfileParser -> IO ()
createLogfileParser dbConfig (BM.LogfileParser name parsers) =
  LogFileDb.save dbConfig (toDbLogfileParser logfileParser)

  where logfileParser = BM.LogfileParser name parsers


existingElementaryParsers :: Configs.FileDbConfig -> IO [BM.ElementaryParser]
existingElementaryParsers dbConfig =
  fmap (map fromDbElementaryParser) (ElemFileDb.readAll dbConfig)


createElementaryParser :: Configs.FileDbConfig -> BM.ElementaryParser -> IO ()
createElementaryParser dbConfig elementaryParser =
  ElemFileDb.save dbConfig (toDbElementaryParser elementaryParser)


applyElementaryParser :: (String, BM.ElementaryParser) -> BM.ElementaryParsingResult
applyElementaryParser ( target, (BM.ElementaryParser name options parser) ) = do
  case parsingResult of
    Left err ->
      BM.ElementaryParsingResult name (BM.ParsingError (show err))

    Right result ->
      BM.ElementaryParsingResult name result

    where parsingResult = ElementaryParsing.applyParser target parser


applyElementaryParserByName :: Configs.FileDbConfig -> String -> String -> IO BM.ElementaryParsingResult
applyElementaryParserByName dbConfig parserName target = do
  parsers <- fmap (map fromDbElementaryParser) (ElemFileDb.readAll dbConfig)
  let (BM.ElementaryParser _ _ parser) = head $ filter byName parsers
  let parsingResult = ElementaryParsing.applyParser target parser

  case parsingResult of
    Left err ->
      return $ BM.ElementaryParsingResult parserName (BM.ParsingError (show err))

    Right result ->
      return $ BM.ElementaryParsingResult parserName result

    where byName (BM.ElementaryParser name _ _) = name == parserName


applyLogfileParser :: (String, BM.LogfileParser) -> BM.LogfileParsingResult
applyLogfileParser ( target, (BM.LogfileParser name parsers) ) = do
  case parsingResult of
    Left err ->
      BM.LogfileParsingError (show err)

    Right result ->
      result

  where logfileParser = BM.LogfileParser name parsers
        parsingResult = LogfileParsing.applyLogfileParser target logfileParser


applyLogfileParserToFile :: Configs.FileDbConfig -> (String, FilePath) -> IO BM.LogfileParsingResult
applyLogfileParserToFile dbConfig (parserName, logfilePath) = do
  parsers <- LogFileDb.readAll dbConfig

  target <- readFile logfilePath

  let parser = head $ filter byName (map fromDbLogfileParser parsers)
  let parsingResult = LogfileParsing.applyLogfileParser target parser

  case parsingResult of
    Left err ->
      return $ BM.LogfileParsingError (show err)

    Right result ->
      return result

  where byName (BM.LogfileParser name _ ) = name == parserName


applyLogfileParserByName :: Configs.FileDbConfig -> (String, String) -> IO BM.LogfileParsingResult
applyLogfileParserByName dbConfig (parserName, target) = do
  parsers <- LogFileDb.readAll dbConfig
  let parser = head $ filter byName (map fromDbLogfileParser parsers)
  let parsingResult = LogfileParsing.applyLogfileParser target parser

  case parsingResult of
    Left err ->
      return $ BM.LogfileParsingError (show err)

    Right result ->
      return result

  where byName (BM.LogfileParser name _ ) = name == parserName


toLogfileParsingResponse :: LogfileParsingResult -> LogfileParsingResponse
toLogfileParsingResponse (LogfileParsingSuccess result) =
    let toParsingResult (NamedParsingResult n r) = ParsingResponse n r
    in  LogfileParsingResponse $ map (map toParsingResult) result
toLogfileParsingResponse (LogfileParsingFailure err) =
    LogfileParsingError err
