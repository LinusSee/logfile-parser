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
  deriving (Show, Read)


instance ToJSON ElementaryParser where
  toJSON (OneOf n xs)     = object [ "type" .= ("oneOf" :: Text),      "name" .= n, "values" .= xs ]
  toJSON (Time n p)       = object [ "type" .= ("time" :: Text),       "name" .= n, "pattern" .= p ]
  toJSON (Date n p)       = object [ "type" .= ("date" :: Text),       "name" .= n, "pattern" .= p ]
  toJSON (Characters n s) = object [ "type" .= ("characters" :: Text), "name" .= n, "value" .= s ]

instance FromJSON ElementaryParser where
  parseJSON (Object o) =
    do parserType <- o .: "type"
       case parserType of String "oneOf"      -> OneOf      <$> o .: "name" <*> o .: "values"
                          String "time"       -> Time       <$> o .: "name" <*> o .: "pattern"
                          String "date"       -> Date       <$> o .: "name" <*> o .: "pattern"
                          String "characters" -> Characters <$> o .: "name" <*> o .: "value"
                          --_                   -> empty


data ParsingResult =
    OneOfResult String
  | TimeResult String
  | DateResult String
  | CharactersResult String
  | ParsingError String


data LogfileParser =
  LogfileParser String [ElementaryParser]
  deriving (Show, Read)

instance FromJSON LogfileParser where
  parseJSON (Object o) =
    LogfileParser <$> o .: "name" <*> o .: "parsers"

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
  toJSON (ParsingResponse name (OneOfResult val))      = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (TimeResult val))       = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (DateResult val))       = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (CharactersResult val)) = object [ "name" .= name, "result" .= val ]
  toJSON (ParsingResponse name (ParsingError val))     = object [ "name" .= name, "error" .= val  ]


data LogfileParsingRequest =
  LogfileParsingRequest String LogfileParser

instance FromJSON LogfileParsingRequest where
  parseJSON (Object o) =
    LogfileParsingRequest <$> o .: "target" <*> o .: "parser"


data LogfileParsingResponse =
    LogfileParsingResponse [ParsingResponse]
  | LogfileParsingError String

instance ToJSON LogfileParsingResponse where
  toJSON (LogfileParsingResponse val) = object [ "result" .= val ]
  toJSON (LogfileParsingError err) = object [ "error" .= err ]
