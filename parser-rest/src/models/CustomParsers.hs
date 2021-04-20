{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CustomParsers
( ElementaryParser (..)
, BasicParser (..)
, LogfileParser (..)
, ParsingResult (..)
, NamedParsingResult (..)
, LogfileParsingResult (..)
, ParsingRequest (..)
, ParsingResponse (..)
, LogfileParsingRequest (..)
, LogfileParsingFileRequest (..)
, LogfileParsingResponse (..)
, CreateLogfileParserRequest (..)
, NamedElementaryParser (..)
, ParsingOptions (..)
, ParsingOption (..)
, fromDbElementaryParser
, toDbElementaryParser
, fromDbLogfileParser
, toDbLogfileParser
, fromRestElementaryParser
, fromElementaryParsingRequest
, toRestElementaryParser
, toRestElementaryParsingResponse
, fromRestCreateLogfileParserRequest
, fromRestLogfileParsingRequest
, fromRestLogfileParsingFileRequest
, toRestLogfileParsingResponse
) where

import Data.Time (TimeOfDay, Day)

import qualified DbParserModels as DM
import qualified RestParserModels as RM



-- MODELS
data ElementaryParser =
  ElementaryParser String ParsingOptions BasicParser
  deriving (Show, Read, Eq)

data ParsingOptions =
  ParsingOptions [ParsingOption]
  deriving (Show, Read, Eq)

data ParsingOption =
  KeepResult Bool
  deriving (Show, Read, Eq)

type TimePattern = String
type DatePattern = String

data BasicParser =
    OneOf [String]
  | Time TimePattern
  | Date DatePattern
  | Characters String
  | MatchUntilIncluded String
  | MatchUntilExcluded String
  | MatchFor Int
  | MatchUntilEnd
  deriving (Show, Read, Eq)


data ParsingResult =
    OneOfResult String
  | TimeResult TimeOfDay
  | DateResult Day
  | CharactersResult String
  | MatchUntilIncludedResult String
  | MatchUntilExcludedResult String
  | MatchForResult String
  | MatchUntilEndResult String
  | ParsingError String
  deriving (Show, Read, Eq)


data NamedParsingResult =
    NamedParsingResult String ParsingResult
    deriving (Show, Read, Eq)


data LogfileParser =
  LogfileParser String [NamedElementaryParser]
  deriving (Show, Read, Eq)


data LogfileParsingResult =
    LogfileParsingSuccess [[NamedParsingResult]]
  | LogfileParsingFailure String
  deriving (Show, Read, Eq)


data CreateLogfileParserRequest =
  CreateLogfileParserRequest String [ NamedElementaryParser ]


data NamedElementaryParser =
    NamedElementaryParser String ElementaryParser
    deriving (Show, Read, Eq)


data ParsingRequest =
    ParsingRequest String ElementaryParser


data ParsingResponse =
  ParsingResponse String ParsingResult
  deriving (Eq, Show, Read)


data LogfileParsingRequest =
  LogfileParsingRequest String CreateLogfileParserRequest


data LogfileParsingFileRequest =
  LogfileParsingFileRequest { name :: String
                            , logfile :: FilePath
                            }
  deriving (Show)


data LogfileParsingResponse =
    LogfileParsingResponse [[ParsingResponse]]
  | LogfileParsingError String
  deriving (Eq, Show)


fromDbElementaryParser :: DM.ElementaryParser -> ElementaryParser
fromDbElementaryParser (DM.ElementaryParser name options parserType) =
  ElementaryParser
          name
          (fromDbParsingOptions options)
          (fromDbParserType parserType)


fromDbParsingOptions :: DM.ParsingOptions -> ParsingOptions
fromDbParsingOptions (DM.ParsingOptions keepResult) =
  ParsingOptions [KeepResult keepResult]

fromDbParserType :: DM.ParserType -> BasicParser
fromDbParserType parserType =
  case parserType of
    DM.OneOf xs             -> OneOf xs
    DM.Time pattern         -> Time pattern
    DM.Date pattern         -> Date pattern
    DM.Characters x         -> Characters x
    DM.MatchUntilIncluded x -> MatchUntilIncluded x
    DM.MatchUntilExcluded x -> MatchUntilExcluded x
    DM.MatchFor c           -> MatchFor c
    DM.MatchUntilEnd        -> MatchUntilEnd


toDbElementaryParser :: ElementaryParser -> DM.ElementaryParser
toDbElementaryParser (ElementaryParser name options parserType) =
  DM.ElementaryParser
          name
          (toDbParsingOptions options)
          (toDbParserType parserType)

toDbParsingOptions :: ParsingOptions -> DM.ParsingOptions
toDbParsingOptions options = DM.ParsingOptions {DM.keepResult = True}

toDbParserType :: BasicParser -> DM.ParserType
toDbParserType parserType =
  case parserType of
    OneOf xs             -> DM.OneOf xs
    Time pattern         -> DM.Time pattern
    Date pattern         -> DM.Date pattern
    Characters x         -> DM.Characters x
    MatchUntilIncluded x -> DM.MatchUntilIncluded x
    MatchUntilExcluded x -> DM.MatchUntilExcluded x
    MatchFor c           -> DM.MatchFor c
    MatchUntilEnd        -> DM.MatchUntilEnd


fromDbLogfileParser :: DM.LogfileParser -> LogfileParser
fromDbLogfileParser (DM.LogfileParser name namedParsers) =
  LogfileParser name (map fromDbNamedParser namedParsers)

fromDbNamedParser :: DM.NamedElementaryParser -> NamedElementaryParser
fromDbNamedParser (DM.NamedElementaryParser name parser) =
  NamedElementaryParser name (fromDbElementaryParser parser)


toDbLogfileParser :: LogfileParser -> DM.LogfileParser
toDbLogfileParser (LogfileParser name namedParsers) =
  DM.LogfileParser name (map toDbNamedParser namedParsers)

toDbNamedParser :: NamedElementaryParser -> DM.NamedElementaryParser
toDbNamedParser (NamedElementaryParser name parser) =
  DM.NamedElementaryParser name (toDbElementaryParser parser)


fromRestElementaryParser :: RM.ElementaryParser -> ElementaryParser
fromRestElementaryParser (RM.ElementaryParser name options parserType) =
  ElementaryParser
          name
          (fromRestParsingOptions options)
          (fromRestParserType parserType)

fromRestParsingOptions :: RM.ParsingOptions -> ParsingOptions
fromRestParsingOptions options = ParsingOptions [KeepResult True]


fromRestParserType :: RM.ParserType -> BasicParser
fromRestParserType parserType =
  case parserType of
    RM.OneOf xs             -> OneOf xs
    RM.Time pattern         -> Time pattern
    RM.Date pattern         -> Date pattern
    RM.Characters x         -> Characters x
    RM.MatchUntilIncluded x -> MatchUntilIncluded x
    RM.MatchUntilExcluded x -> MatchUntilExcluded x
    RM.MatchFor c           -> MatchFor c
    RM.MatchUntilEnd        -> MatchUntilEnd

fromElementaryParsingRequest :: RM.ElementaryParsingRequest -> ParsingRequest
fromElementaryParsingRequest (RM.ElementaryParsingRequest target parser) =
  ParsingRequest
          target
          (fromRestElementaryParser parser)

toRestElementaryParser :: ElementaryParser -> RM.ElementaryParser
toRestElementaryParser (ElementaryParser name options parserType) =
  RM.ElementaryParser
          name
          (toRestParsingOptions options)
          (toRestParserType parserType)

toRestParsingOptions :: ParsingOptions -> RM.ParsingOptions
toRestParsingOptions options = RM.ParsingOptions { RM.keepResult = True }

toRestParserType :: BasicParser -> RM.ParserType
toRestParserType parserType =
  case parserType of
    OneOf xs             -> RM.OneOf xs
    Time pattern         -> RM.Time pattern
    Date pattern         -> RM.Date pattern
    Characters x         -> RM.Characters x
    MatchUntilIncluded x -> RM.MatchUntilIncluded x
    MatchUntilExcluded x -> RM.MatchUntilExcluded x
    MatchFor c           -> RM.MatchFor c
    MatchUntilEnd        -> RM.MatchUntilEnd


toRestElementaryParsingResponse :: ParsingResponse -> RM.ElementaryParsingResponse
toRestElementaryParsingResponse (ParsingResponse name result) =
  RM.ElementaryParsingResponse
            name
            (toRestElementaryParsingResult result)

toRestElementaryParsingResult :: ParsingResult -> RM.ParsingResult
toRestElementaryParsingResult parsingResult =
  case parsingResult of
    OneOfResult result              -> RM.OneOfResult result
    TimeResult result               -> RM.TimeResult result
    DateResult result               -> RM.DateResult result
    CharactersResult result         -> RM.CharactersResult result
    MatchUntilIncludedResult result -> RM.MatchUntilIncludedResult result
    MatchUntilExcludedResult result -> RM.MatchUntilExcludedResult result
    MatchForResult result           -> RM.MatchForResult result
    MatchUntilEndResult result      -> RM.MatchUntilEndResult result
    ParsingError err                -> RM.ParsingError err


fromRestCreateLogfileParserRequest :: RM.CreateLogfileParserRequest -> CreateLogfileParserRequest
fromRestCreateLogfileParserRequest (RM.LogfileParser name namedParsers) =
  CreateLogfileParserRequest
            name
            (map fromRestNamedParser namedParsers)


fromRestNamedParser :: RM.NamedElementaryParser -> NamedElementaryParser
fromRestNamedParser (RM.NamedElementaryParser name parser) =
  NamedElementaryParser
            name
            (fromRestElementaryParser parser)

fromRestLogfileParsingRequest :: RM.LogfileParsingRequest -> LogfileParsingRequest
fromRestLogfileParsingRequest (RM.LogfileParsingRequest target parser) =
  LogfileParsingRequest
            target
            (fromRestCreateLogfileParserRequest parser)

fromRestLogfileParsingFileRequest :: RM.LogfileParsingFileRequest -> LogfileParsingFileRequest
fromRestLogfileParsingFileRequest (RM.LogfileParsingFileRequest name logfilePath) =
  LogfileParsingFileRequest name logfilePath

toRestLogfileParsingResponse :: LogfileParsingResponse -> RM.LogfileParsingResponse
toRestLogfileParsingResponse (LogfileParsingResponse responses) =
  RM.LogfileParsingResponse $ map (map toRestElementaryParsingResponse) responses
toRestLogfileParsingResponse (LogfileParsingError err) =
  RM.LogfileParsingError err


-- fromRestLogfileParser :: RM.LogfileParser -> LogfileParser
-- fromRestLogfileParser (RM.LogfileParser name namedParsers) =
--   LogfileParser name (map fromDbNamedParser namedParsers)
--
-- fromRestNamedParser :: RM.NamedElementaryParser -> NamedElementaryParser
-- fromRestNamedParser (RM.NamedElementaryParser name parser) =
--   NamedElementaryParser name (fromDbElementaryParser parser)
--
--
-- toRestLogfileParser :: LogfileParser -> RM.LogfileParser
-- toRestLogfileParser (LogfileParser name namedParsers) =
--   RM.LogfileParser name (map toDbNamedParser namedParsers)
--
-- toRestNamedParser :: NamedElementaryParser -> RM.NamedElementaryParser
-- toRestNamedParser (NamedElementaryParser name parser) =
--   RM.NamedElementaryParser name (toDbElementaryParser parser)
