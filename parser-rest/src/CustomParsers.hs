{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CustomParsers
( ElementaryParser (..)
, LogfileParser (..)
, ParsingRequest (..)
, ParsingResponse (..)
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
    OneOfResponse String
  | TimeResponse String -- TODO: Will be some time format
  | DateResponse String -- TODO: Will be some date format
  | CharactersResponse String
  | ParsingError String

instance ToJSON ParsingResponse where
  toJSON (OneOfResponse val)      = object [ "result" .= val ]
  toJSON (TimeResponse val)       = object [ "result" .= val ]
  toJSON (DateResponse val)       = object [ "result" .= val ]
  toJSON (CharactersResponse val) = object [ "result" .= val ]
  toJSON (ParsingError val) = object [ "error" .= val ]
