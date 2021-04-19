{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module RestParserModels
( ElementaryParser (..)
, ParsingOptions (..)
, ParserType (..)
) where

import Data.Aeson
import Data.Time (TimeOfDay, Day)
import Data.Text (Text)


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


instance ToJSON ElementaryParser where
  toJSON (ElementaryParser n os (OneOf xs))             = object [ "type" .= ("oneOf" :: Text),               "name" .= n, "options" .= os, "values" .= xs ]
  toJSON (ElementaryParser n os (Time p))               = object [ "type" .= ("time" :: Text),                "name" .= n, "options" .= os, "pattern" .= p ]
  toJSON (ElementaryParser n os (Date p))               = object [ "type" .= ("date" :: Text),                "name" .= n, "options" .= os, "pattern" .= p ]
  toJSON (ElementaryParser n os (Characters s))         = object [ "type" .= ("characters" :: Text),          "name" .= n, "options" .= os, "value" .= s ]
  toJSON (ElementaryParser n os (MatchUntilIncluded s)) = object [ "type" .= ("matchUntilIncluded" :: Text),  "name" .= n, "options" .= os, "value" .= s ]
  toJSON (ElementaryParser n os (MatchUntilExcluded s)) = object [ "type" .= ("matchUntilExcluded" :: Text),  "name" .= n, "options" .= os, "value" .= s ]
  toJSON (ElementaryParser n os (MatchFor i))           = object [ "type" .= ("matchFor" :: Text),            "name" .= n, "options" .= os, "count" .= i ]
  toJSON (ElementaryParser n os MatchUntilEnd)          = object [ "type" .= ("matchUntilEnd" :: Text),       "name" .= ("matchUntilEnd" :: Text), "options" .= os ]

instance ToJSON ParsingOptions where
  toJSON ParsingOptions{keepResult=keepResult} = object [ "keepResult" .= keepResult ]


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
