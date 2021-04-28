{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module ModelMapping
( fromDbElementaryParser
, fromDbElementaryParserId
, toDbElementaryParser
, fromDbLogfileParser
, toDbLogfileParser
, fromRestElementaryParser
, fromElementaryParsingRequest
, toRestElementaryParser
, toRestElementaryParserId
, toRestElementaryParsingResponse
, fromRestLogfileParser
, fromRestCreateLogfileParserRequest
, fromRestLogfileParsingRequest
, fromRestLogfileParsingFileRequest
, toRestLogfileParsingResponse
) where

import Data.Time (TimeOfDay, Day)

import qualified BusinessLogicModels as BM
import qualified DbParserModels as DM
import qualified RestParserModels as RM



fromDbElementaryParser :: DM.ElementaryParser -> BM.ElementaryParser
fromDbElementaryParser (DM.ElementaryParser name options parserType) =
  BM.ElementaryParser
          name
          (fromDbParsingOptions options)
          (fromDbParserType parserType)


fromDbElementaryParserId :: DM.ElementaryParserId -> BM.ElementaryParserId
fromDbElementaryParserId (DM.ElementaryParserId uuid name) =
  BM.ElementaryParserId uuid name


fromDbParsingOptions :: DM.ParsingOptions -> BM.ParsingOptions
fromDbParsingOptions (DM.ParsingOptions keepResult) =
  BM.ParsingOptions { BM.keepResult = keepResult
                    }

fromDbParserType :: DM.ParserType -> BM.ParserType
fromDbParserType parserType =
  case parserType of
    DM.OneOf xs             -> BM.OneOf xs
    DM.Time pattern         -> BM.Time pattern
    DM.Date pattern         -> BM.Date pattern
    DM.Characters x         -> BM.Characters x
    DM.MatchUntilIncluded x -> BM.MatchUntilIncluded x
    DM.MatchUntilExcluded x -> BM.MatchUntilExcluded x
    DM.MatchFor c           -> BM.MatchFor c
    DM.MatchUntilEnd        -> BM.MatchUntilEnd


toDbElementaryParser :: BM.ElementaryParser -> DM.ElementaryParser
toDbElementaryParser (BM.ElementaryParser name options parserType) =
  DM.ElementaryParser
          name
          (toDbParsingOptions options)
          (toDbParserType parserType)

toDbParsingOptions :: BM.ParsingOptions -> DM.ParsingOptions
toDbParsingOptions options = DM.ParsingOptions { DM.keepResult = BM.keepResult options
                                               }

toDbParserType :: BM.ParserType -> DM.ParserType
toDbParserType parserType =
  case parserType of
    BM.OneOf xs             -> DM.OneOf xs
    BM.Time pattern         -> DM.Time pattern
    BM.Date pattern         -> DM.Date pattern
    BM.Characters x         -> DM.Characters x
    BM.MatchUntilIncluded x -> DM.MatchUntilIncluded x
    BM.MatchUntilExcluded x -> DM.MatchUntilExcluded x
    BM.MatchFor c           -> DM.MatchFor c
    BM.MatchUntilEnd        -> DM.MatchUntilEnd


fromDbLogfileParser :: DM.LogfileParser -> BM.LogfileParser
fromDbLogfileParser (DM.LogfileParser name namedParsers) =
  BM.LogfileParser name (map fromDbNamedParser namedParsers)

fromDbNamedParser :: DM.NamedElementaryParser -> BM.NamedElementaryParser
fromDbNamedParser (DM.NamedElementaryParser name parser) =
  BM.NamedElementaryParser name (fromDbElementaryParser parser)


toDbLogfileParser :: BM.LogfileParser -> DM.LogfileParser
toDbLogfileParser (BM.LogfileParser name namedParsers) =
  DM.LogfileParser name (map toDbNamedParser namedParsers)

toDbNamedParser :: BM.NamedElementaryParser -> DM.NamedElementaryParser
toDbNamedParser (BM.NamedElementaryParser name parser) =
  DM.NamedElementaryParser name (toDbElementaryParser parser)


fromRestElementaryParser :: RM.ElementaryParser -> BM.ElementaryParser
fromRestElementaryParser (RM.ElementaryParser name options parserType) =
  BM.ElementaryParser
          name
          (fromRestParsingOptions options)
          (fromRestParserType parserType)

fromRestParsingOptions :: RM.ParsingOptions -> BM.ParsingOptions
fromRestParsingOptions options =
  BM.ParsingOptions { BM.keepResult = RM.keepResult options
                    }


fromRestParserType :: RM.ParserType -> BM.ParserType
fromRestParserType parserType =
  case parserType of
    RM.OneOf xs             -> BM.OneOf xs
    RM.Time pattern         -> BM.Time pattern
    RM.Date pattern         -> BM.Date pattern
    RM.Characters x         -> BM.Characters x
    RM.MatchUntilIncluded x -> BM.MatchUntilIncluded x
    RM.MatchUntilExcluded x -> BM.MatchUntilExcluded x
    RM.MatchFor c           -> BM.MatchFor c
    RM.MatchUntilEnd        -> BM.MatchUntilEnd

fromElementaryParsingRequest :: RM.ElementaryParsingRequest -> (String, BM.ElementaryParser)
fromElementaryParsingRequest (RM.ElementaryParsingRequest target parser) =
  ( target, fromRestElementaryParser parser)

toRestElementaryParser :: BM.ElementaryParser -> RM.ElementaryParser
toRestElementaryParser (BM.ElementaryParser name options parserType) =
  RM.ElementaryParser
          name
          (toRestParsingOptions options)
          (toRestParserType parserType)

toRestElementaryParserId :: BM.ElementaryParserId -> RM.ElementaryParserId
toRestElementaryParserId (BM.ElementaryParserId uuid name) =
  RM.ElementaryParserId uuid name


toRestParsingOptions :: BM.ParsingOptions -> RM.ParsingOptions
toRestParsingOptions options = RM.ParsingOptions { RM.keepResult = BM.keepResult options }

toRestParserType :: BM.ParserType -> RM.ParserType
toRestParserType parserType =
  case parserType of
    BM.OneOf xs             -> RM.OneOf xs
    BM.Time pattern         -> RM.Time pattern
    BM.Date pattern         -> RM.Date pattern
    BM.Characters x         -> RM.Characters x
    BM.MatchUntilIncluded x -> RM.MatchUntilIncluded x
    BM.MatchUntilExcluded x -> RM.MatchUntilExcluded x
    BM.MatchFor c           -> RM.MatchFor c
    BM.MatchUntilEnd        -> RM.MatchUntilEnd


toRestElementaryParsingResponse :: BM.ElementaryParsingResult -> RM.ElementaryParsingResponse
toRestElementaryParsingResponse (BM.ElementaryParsingResult name result) =
  RM.ElementaryParsingResponse
            name
            (toRestElementaryParsingResult result)

toRestElementaryParsingResult :: BM.ParsingResultType -> RM.ParsingResultType
toRestElementaryParsingResult parsingResult =
  case parsingResult of
    BM.OneOfResult result              -> RM.OneOfResult result
    BM.TimeResult result               -> RM.TimeResult result
    BM.DateResult result               -> RM.DateResult result
    BM.CharactersResult result         -> RM.CharactersResult result
    BM.MatchUntilIncludedResult result -> RM.MatchUntilIncludedResult result
    BM.MatchUntilExcludedResult result -> RM.MatchUntilExcludedResult result
    BM.MatchForResult result           -> RM.MatchForResult result
    BM.MatchUntilEndResult result      -> RM.MatchUntilEndResult result
    BM.ParsingError err                -> RM.ParsingError err


fromRestLogfileParser :: RM.LogfileParser -> BM.LogfileParser
fromRestLogfileParser (RM.LogfileParser name namedParsers) =
  BM.LogfileParser
            name
            (map fromRestNamedParser namedParsers)

fromRestCreateLogfileParserRequest :: RM.CreateLogfileParserRequest -> BM.LogfileParser
fromRestCreateLogfileParserRequest = fromRestLogfileParser


fromRestNamedParser :: RM.NamedElementaryParser -> BM.NamedElementaryParser
fromRestNamedParser (RM.NamedElementaryParser name parser) =
  BM.NamedElementaryParser
            name
            (fromRestElementaryParser parser)

fromRestLogfileParsingRequest :: RM.LogfileParsingRequest -> (String, BM.LogfileParser)
fromRestLogfileParsingRequest (RM.LogfileParsingRequest target parser) =
  ( target, fromRestCreateLogfileParserRequest parser)

fromRestLogfileParsingFileRequest :: RM.LogfileParsingFileRequest -> (String, FilePath)
fromRestLogfileParsingFileRequest (RM.LogfileParsingFileRequest name logfilePath) =
  (name, logfilePath)

toRestLogfileParsingResponse :: BM.LogfileParsingResult -> RM.LogfileParsingResponse
toRestLogfileParsingResponse (BM.LogfileParsingResult results) =
  RM.LogfileParsingResponse $ map (map toRestElementaryParsingResponse) results
toRestLogfileParsingResponse (BM.LogfileParsingError err) =
  RM.LogfileParsingError err
