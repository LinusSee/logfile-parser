{-# LANGUAGE DuplicateRecordFields #-}

module BusinessLogicModels
( ElementaryParser (..)
, ParsingOptions (..)
, ParserType (..)
, ElementaryParsingResult (..)
, ParsingResultType (..)
, LogfileParser (..)
, NamedElementaryParser (..)
, LogfileParsingResult (..)
) where

import Data.Time (TimeOfDay, Day)


data ElementaryParser =
  ElementaryParser { name :: String
                   , options :: ParsingOptions
                   , parserType :: ParserType
                   }
  deriving (Show, Read, Eq)


data ParsingOptions =
  ParsingOptions { keepResult :: Bool
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
  deriving (Show, Read, Eq)

type TimePattern = String
type DatePattern = String



data LogfileParser =
  LogfileParser { name :: String
                , namedParsers :: [NamedElementaryParser]
                }
  deriving (Show, Read, Eq)


data NamedElementaryParser =
  NamedElementaryParser { name :: String
                        , parser :: ElementaryParser
                        }
  deriving (Show, Read, Eq)



data ElementaryParsingResult =
  ElementaryParsingResult { name :: String
                          , resultType :: ParsingResultType
                          }
  deriving (Show, Read, Eq)

data ParsingResultType =
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


data LogfileParsingResult =
  LogfileParsingResult [[ElementaryParsingResult]]
  deriving (Show, Read, Eq)

-- data NamedElementaryParsingResult =
--     NamedElementaryParsingResult { name :: String
--                                  , result :: ElementaryParsingResult
--                                  }
--     deriving (Show, Read, Eq)
