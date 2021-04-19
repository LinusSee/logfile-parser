{-# LANGUAGE DuplicateRecordFields #-}

module RestParserModels
(
) where

import Data.Time (TimeOfDay, Day)


type CreateElementaryParserRequest = ElementaryParser

data ElementaryParser =
  ElementaryParser { name :: String
                   , options :: ParsingOptions
                   , parserType :: ParserType
                   }
  deriving (Show, Read, Eq)

data ParserType =
    OneOf [String]
  | Time TimePattern
  | Date DatePattern
  | Characters String
  | MatchUntilIncluded String
  | MatchUntilExcluded String
  | MatchFor Int
  | MatchUntilEnd
  | InvalidParser String
  deriving (Show, Read, Eq)

type TimePattern = String
type DatePattern = String

data ParsingOptions =
  ParsingOptions { keepResult :: Bool
                 }
  deriving (Show, Read, Eq)


data NamedElementaryParser =
    NamedElementaryParser String ElementaryParser
    deriving (Show, Read, Eq)


data ElementaryParsingRequest =
  ElementaryParsingRequest { target :: String
                           , parser :: ElementaryParser
                           }
  deriving (Show, Read, Eq)


data ElementaryParsingResponse =
  ElementaryParsingResponse { name :: String
                            , result :: ParsingResult
                            }
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


type CreateLogfileParserRequest = LogfileParser


data LogfileParser =
  LogfileParser { name :: String
                 , namedParsers :: [ NamedElementaryParser ]
                 }


data LogfileParsingRequest =
  LogfileParsingRequest { target :: String
                        , parser :: LogfileParser
                        }


data LogfileParsingResponse =
    LogfileParsingResponse [[ElementaryParsingResponse]]
  | LogfileParsingError String
  deriving (Eq, Show)
