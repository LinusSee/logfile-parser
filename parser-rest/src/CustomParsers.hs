{-# LANGUAGE OverloadedStrings #-}
module CustomParsers (
  ElementaryParser(..)
) where

import Data.Aeson
import Data.Text (Text)

type TimePattern = String
type DatePattern = String

data ElementaryParser =
    OneOf [String]
  | Time TimePattern
  | Date DatePattern
  | Characters String

oneOfType :: Text
oneOfType = "oneOf"

timeType :: Text
timeType = "time"

dateType :: Text
dateType = "date"

charactersType :: Text
charactersType = "characters"

instance ToJSON ElementaryParser where
  toJSON (OneOf xs)     = object [ "type" .= oneOfType,      "values" .= xs ]
  toJSON (Time p)       = object [ "type" .= timeType,       "pattern" .= p ]
  toJSON (Date p)       = object [ "type" .= dateType,       "pattern" .= p ]
  toJSON (Characters s) = object [ "type" .= charactersType, "value" .= s ]
