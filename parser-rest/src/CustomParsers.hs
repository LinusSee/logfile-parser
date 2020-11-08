{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CustomParsers
( ElementaryParser (..)
, ParsingRequest (..)
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

type TimePattern = String
type DatePattern = String



-- MODELS


data ElementaryParser =
    OneOf [String]
  | Time TimePattern
  | Date DatePattern
  | Characters String
  deriving (Show, Read)


instance ToJSON ElementaryParser where
  toJSON (OneOf xs)     = object [ "type" .= ("oneOf" :: Text),      "values" .= xs ]
  toJSON (Time p)       = object [ "type" .= ("time" :: Text),       "pattern" .= p ]
  toJSON (Date p)       = object [ "type" .= ("date" :: Text),       "pattern" .= p ]
  toJSON (Characters s) = object [ "type" .= ("characters" :: Text), "value" .= s ]

instance FromJSON ElementaryParser where
  parseJSON (Object o) =
    do parserType <- o .: "type"
       case parserType of String "oneOf"      -> OneOf      <$> o .: "values"
                          String "time"       -> Time       <$> o .: "pattern"
                          String "date"       -> Date       <$> o .: "pattern"
                          String "characters" -> Characters <$> o .: "value"
                          --_                   -> empty


data ParsingRequest =
    ParsingRequest String ElementaryParser

$(deriveJSON defaultOptions ''ParsingRequest)
