{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
, LogfileParsingResponse (..)
, CreateLogfileParserRequest (..)
, NamedElementaryParser (..)
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Time (TimeOfDay, Day)



-- MODELS
data ElementaryParser =
  ElementaryParser String BasicParser
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


instance ToJSON ElementaryParser where
  toJSON (ElementaryParser n (OneOf xs))             = object [ "type" .= ("oneOf" :: Text),               "name" .= n, "values" .= xs ]
  toJSON (ElementaryParser n (Time p))               = object [ "type" .= ("time" :: Text),                "name" .= n, "pattern" .= p ]
  toJSON (ElementaryParser n (Date p))               = object [ "type" .= ("date" :: Text),                "name" .= n, "pattern" .= p ]
  toJSON (ElementaryParser n (Characters s))         = object [ "type" .= ("characters" :: Text),          "name" .= n, "value" .= s ]
  toJSON (ElementaryParser n (MatchUntilIncluded s)) = object [ "type" .= ("matchUntilIncluded" :: Text),  "name" .= n, "value" .= s ]
  toJSON (ElementaryParser n (MatchUntilExcluded s)) = object [ "type" .= ("matchUntilExcluded" :: Text),  "name" .= n, "value" .= s ]
  toJSON (ElementaryParser n (MatchFor i))           = object [ "type" .= ("matchFor" :: Text),            "name" .= n, "count" .= i ]
  toJSON (ElementaryParser n MatchUntilEnd)          = object [ "type" .= ("matchUntilEnd" :: Text),       "name" .= ("matchUntilEnd" :: Text) ]


instance FromJSON ElementaryParser where
  parseJSON (Object o) =
    do parserType <- o .: "type"
       case parserType of
          String "oneOf"      ->
            ElementaryParser <$> o.: "name" <*> (fmap OneOf (o .: "values"))

          String "time"       ->
            ElementaryParser <$> o.: "name" <*> (fmap Time (o .: "pattern"))

          String "date"       ->
            ElementaryParser <$> o.: "name" <*> (fmap Date (o .: "pattern"))

          String "characters" ->
            ElementaryParser <$> o.: "name" <*> (fmap Characters (o .: "value"))

          String "matchUntilIncluded" ->
            ElementaryParser <$> o.: "name" <*> (fmap MatchUntilIncluded (o .: "value"))

          String "matchUntilExcluded" ->
            ElementaryParser <$> o.: "name" <*> (fmap MatchUntilExcluded (o .: "value"))

          String "matchFor" ->
            ElementaryParser <$> o.: "name" <*> (fmap MatchFor (o .: "count"))

          String "matchUntilEnd" ->
            -- TODO: Simple return should suffice
            ElementaryParser <$> pure "matchUntilEnd" <*> pure MatchUntilEnd
          --_                   -> empty


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

instance FromJSON LogfileParser where
  parseJSON (Object o) =
    LogfileParser <$> o .: "name" <*> o .: "parsers"


data LogfileParsingResult =
    LogfileParsingSuccess [[NamedParsingResult]]
  | LogfileParsingFailure String
  deriving (Show, Read, Eq)


data CreateLogfileParserRequest =
  CreateLogfileParserRequest String [ NamedElementaryParser ]

instance FromJSON CreateLogfileParserRequest where
  parseJSON (Object o) =
    CreateLogfileParserRequest <$> o .: "name" <*> o .: "parsers"


data NamedElementaryParser =
    NamedElementaryParser String ElementaryParser
    deriving (Show, Read, Eq)

instance FromJSON NamedElementaryParser where
  parseJSON (Object o) =
    NamedElementaryParser <$> o .: "name" <*> o .: "parser"


data ParsingRequest =
    ParsingRequest String ElementaryParser

instance FromJSON ParsingRequest where
  parseJSON (Object o) =
    ParsingRequest <$> o .: "target" <*> o .: "parser"


data ParsingResponse =
  ParsingResponse String ParsingResult
  deriving (Eq, Show, Read)

  --   OneOfResponse String
  -- | TimeResponse String -- TODO: Will be some time format
  -- | DateResponse String -- TODO: Will be some date format
  -- | CharactersResponse String
  -- | ParsingError String

instance ToJSON ParsingResponse where
  toJSON (ParsingResponse name (OneOfResult val))               = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (TimeResult val))                = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (DateResult val))                = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (CharactersResult val))          = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (MatchUntilIncludedResult val))  = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (MatchUntilExcludedResult val))  = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (MatchForResult val))            = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (MatchUntilEndResult val))       = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (ParsingError val))              = object [ "name" .= name, "error" .= val  ]


data LogfileParsingRequest =
  LogfileParsingRequest String CreateLogfileParserRequest

instance FromJSON LogfileParsingRequest where
  parseJSON (Object o) =
    LogfileParsingRequest <$> o .: "target" <*> o .: "parser"


data LogfileParsingResponse =
    LogfileParsingResponse [[ParsingResponse]]
  | LogfileParsingError String
  deriving (Eq, Show)

instance ToJSON LogfileParsingResponse where
  toJSON (LogfileParsingResponse val) = object [ "result" .= val ]
  toJSON (LogfileParsingError err) = object [ "error" .= err ]
