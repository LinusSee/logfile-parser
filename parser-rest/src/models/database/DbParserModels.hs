{-# LANGUAGE DuplicateRecordFields #-}

module DbParserModels
( Entity(..)
, ElementaryParser(..)
, ElementaryParserId(..)
, ParsingOptions(..)
, ParserType(..)
, NamedElementaryParser(..)
, LogfileParser(..)
) where

import Data.UUID


data Entity a =
  Entity { id :: UUID
         , entity :: a
         }
  deriving (Show, Read, Eq)

data ElementaryParser =
  ElementaryParser { name :: String
                   , options :: ParsingOptions
                   , parserType :: ParserType
                   }
  deriving (Show, Read, Eq)


data ElementaryParserId =
  ElementaryParserId { id :: UUID
                     , name :: String
                     }


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
