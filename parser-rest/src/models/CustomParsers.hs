{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CustomParsers
( ElementaryParser (..)
, LogfileParser (..)
, ParsingResult (..)
, ParsingRequest (..)
, ParsingResponse (..)
, LogfileParsingRequest (..)
, LogfileParsingResponse (..)
, CreateLogfileParserRequest (..)
, NamedParser (..)
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

type TimePattern = String
type DatePattern = String



-- MODELS


data ElementaryParser =
    OneOf String [String]
  | Time String TimePattern
  | Date String DatePattern
  | Characters String String
  | MatchUntilIncluded String String
  | MatchUntilExcluded String String
  | MatchFor String Int
  | MatchUntilEnd String
  deriving (Show, Read)


instance ToJSON ElementaryParser where
  toJSON (OneOf n xs)             = object [ "type" .= ("oneOf" :: Text),               "name" .= n, "values" .= xs ]
  toJSON (Time n p)               = object [ "type" .= ("time" :: Text),                "name" .= n, "pattern" .= p ]
  toJSON (Date n p)               = object [ "type" .= ("date" :: Text),                "name" .= n, "pattern" .= p ]
  toJSON (Characters n s)         = object [ "type" .= ("characters" :: Text),          "name" .= n, "value" .= s ]
  toJSON (MatchUntilIncluded n s) = object [ "type" .= ("matchUntilIncluded" :: Text),  "name" .= n, "value" .= s ]
  toJSON (MatchUntilExcluded n s) = object [ "type" .= ("matchUntilExcluded" :: Text),  "name" .= n, "value" .= s ]
  toJSON (MatchFor n i)           = object [ "type" .= ("matchFor" :: Text),            "name" .= n, "value" .= i ]
  toJSON (MatchUntilEnd _)          = object [ "type" .= ("matchUntilEnd" :: Text),       "name" .= ("matchUntilEnd" :: Text) ]

instance FromJSON ElementaryParser where
  parseJSON (Object o) =
    do parserType <- o .: "type"
       case parserType of String "oneOf"      -> OneOf      <$> o .: "name" <*> o .: "values"
                          String "time"       -> Time       <$> o .: "name" <*> o .: "pattern"
                          String "date"       -> Date       <$> o .: "name" <*> o .: "pattern"
                          String "characters" -> Characters <$> o .: "name" <*> o .: "value"
                          String "matchUntilIncluded" -> MatchUntilIncluded <$> o.: "name" <*> o .: "value"
                          String "matchUntilExcluded" -> MatchUntilExcluded <$> o.: "name" <*> o .: "value"
                          String "matchFor" -> MatchFor <$> o.: "name" <*> o .: "value"
                          String "matchUntilEnd" -> MatchUntilEnd <$> pure "matchUntilEnd"
                          --_                   -> empty


data ParsingResult =
    OneOfResult String
  | TimeResult String
  | DateResult String
  | CharactersResult String
  | MatchUntilIncludedResult String
  | MatchUntilExcludedResult String
  | MatchForResult String
  | MatchUntilEndResult String
  | ParsingError String


data LogfileParser =
  LogfileParser String [(String, ElementaryParser)]
  deriving (Show, Read)

instance FromJSON LogfileParser where
  parseJSON (Object o) =
    LogfileParser <$> o .: "name" <*> o .: "parsers"


data CreateLogfileParserRequest =
  CreateLogfileParserRequest String [ NamedParser ]

instance FromJSON CreateLogfileParserRequest where
  parseJSON (Object o) =
    CreateLogfileParserRequest <$> o .: "name" <*> o .: "parsers"


data NamedParser = NamedParser String ElementaryParser

instance FromJSON NamedParser where
  parseJSON (Object o) =
    NamedParser <$> o .: "name" <*> o .: "parser"


data ParsingRequest =
    ParsingRequest String ElementaryParser

instance FromJSON ParsingRequest where
  parseJSON (Object o) =
    ParsingRequest <$> o .: "target" <*> o .: "parser"


data ParsingResponse =
  ParsingResponse String ParsingResult

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

instance ToJSON LogfileParsingResponse where
  toJSON (LogfileParsingResponse val) = object [ "result" .= val ]
  toJSON (LogfileParsingError err) = object [ "error" .= err ]
