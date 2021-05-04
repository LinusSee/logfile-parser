module ParsingOrchestration
( existingLogfileParserIds
, existingLogfileParserNames
, createLogfileParser
, existingElementaryParserIds
, existingElementaryParsers
, createElementaryParser
, applyElementaryParser
, applyElementaryParserById
, applyLogfileParser
, applyLogfileParserToFile
, applyLogfileParserById
) where

import Data.UUID

import qualified Configs as Configs
import qualified ElementaryParsing as ElementaryParsing
import qualified ElementaryParserFileDb as ElemFileDb
import qualified LogfileParsing as LogfileParsing
import qualified LogfileParserFileDb as LogFileDb

import qualified BusinessLogicModels as BM
import qualified ModelMapping as MM



existingLogfileParserIds :: Configs.FileDbConfig -> IO [BM.LogfileParserId]
existingLogfileParserIds dbConfig =
  fmap (map MM.fromDbLogfileParserId) (LogFileDb.readAllIds dbConfig)


-- TODO: Remove when existingLogfileParserIds is finished
existingLogfileParserNames :: Configs.FileDbConfig -> IO [String]
existingLogfileParserNames dbConfig = do
  parsers <- LogFileDb.readAll dbConfig

  return $ map extractName (map MM.fromDbLogfileParser parsers)

  where extractName ( BM.LogfileParser name _ ) = name


createLogfileParser :: Configs.FileDbConfig -> BM.LogfileParser -> IO UUID
createLogfileParser dbConfig (BM.LogfileParser name parsers) =
  LogFileDb.save dbConfig (MM.toDbLogfileParser logfileParser)

  where logfileParser = BM.LogfileParser name parsers


existingElementaryParserIds :: Configs.FileDbConfig -> IO [BM.ElementaryParserId]
existingElementaryParserIds dbConfig =
  fmap (map MM.fromDbElementaryParserId) (ElemFileDb.readAllIds dbConfig)


existingElementaryParsers :: Configs.FileDbConfig -> IO [BM.ElementaryParser]
existingElementaryParsers dbConfig =
  fmap (map MM.fromDbElementaryParser) (ElemFileDb.readAll dbConfig)


createElementaryParser :: Configs.FileDbConfig -> BM.ElementaryParser -> IO UUID
createElementaryParser dbConfig elementaryParser =
  ElemFileDb.save dbConfig (MM.toDbElementaryParser elementaryParser)


applyElementaryParser :: (String, BM.ElementaryParser) -> BM.ElementaryParsingResult
applyElementaryParser ( target, (BM.ElementaryParser name options parser) ) = do
  case parsingResult of
    Left err ->
      BM.ElementaryParsingResult name (BM.ParsingError (show err))

    Right result ->
      BM.ElementaryParsingResult name result

    where parsingResult = ElementaryParsing.applyParser target parser


applyElementaryParserById :: Configs.FileDbConfig -> UUID -> String -> IO BM.ElementaryParsingResult
applyElementaryParserById dbConfig uuid target = do
  parser <- fmap (fmap MM.fromDbElementaryParser) (ElemFileDb.readById dbConfig uuid)
  case parser of
    Just (BM.ElementaryParser parserName _ parser) -> do
      let parsingResult = ElementaryParsing.applyParser target parser

      case parsingResult of
        Left err ->
          return $ BM.ElementaryParsingResult parserName (BM.ParsingError (show err))

        Right result ->
          return $ BM.ElementaryParsingResult parserName result

    Nothing ->
      -- TODO: Somehow use problem detail (Might need some rethinking in how to generally handle errors)
      return $ BM.ElementaryParsingResult "Parser not found" (BM.ParsingError ("No parser with UUID " ++ show uuid ++ " was found."))


applyLogfileParser :: (String, BM.LogfileParser) -> BM.LogfileParsingResult
applyLogfileParser ( target, (BM.LogfileParser name parsers) ) = do
  case parsingResult of
    Left err ->
      BM.LogfileParsingError (show err)

    Right result ->
      result

  where logfileParser = BM.LogfileParser name parsers
        parsingResult = LogfileParsing.applyLogfileParser target logfileParser


applyLogfileParserToFile :: Configs.FileDbConfig -> (UUID, FilePath) -> IO BM.LogfileParsingResult
applyLogfileParserToFile dbConfig (uuid, logfilePath) = do
  target <- readFile logfilePath

  applyLogfileParserById dbConfig (uuid, target)


applyLogfileParserById :: Configs.FileDbConfig -> (UUID, String) -> IO BM.LogfileParsingResult
applyLogfileParserById dbConfig (uuid, target) = do
  maybeParser <- LogFileDb.readById dbConfig uuid

  case maybeParser of
    Just parser -> do
      let parsingResult = LogfileParsing.applyLogfileParser target (MM.fromDbLogfileParser parser)

      case parsingResult of
        Left err ->
          return $ BM.LogfileParsingError (show err)

        Right result ->
          return result

    Nothing ->
      return $ BM.LogfileParsingError ("No logfile parser found with uuid [" ++ show uuid ++ "]")
