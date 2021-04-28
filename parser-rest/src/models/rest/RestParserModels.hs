{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module RestParserModels
( ElementaryParser (..)
, ElementaryParserId (..)
, CreateElementaryParserRequest
, ParsingOptions (..)
, ParserType (..)
, ElementaryParsingRequest (..)
, ElementaryParsingResponse (..)
, ParsingResultType (..)
, NamedElementaryParser (..)
, LogfileParser (..)
, CreateLogfileParserRequest (..)
, LogfileParsingRequest (..)
, LogfileParsingFileRequest (..)
, LogfileParsingResponse (..)
) where

import Data.Aeson
import Servant.Multipart
import Data.Time (TimeOfDay, Day)
import Data.Text (Text)
import Data.UUID
import qualified Data.Text as T



-- ElementaryParser Models


type CreateElementaryParserRequest = ElementaryParser

data ElementaryParserId =
  ElementaryParserId { id :: UUID
                     , name :: String
                     }

instance ToJSON ElementaryParserId where
  toJSON (ElementaryParserId idVal name) = object [ "id" .= idVal, "name" .= name ]


data ElementaryParser =
  ElementaryParser { name :: String
                   , options :: ParsingOptions
                   , parserType :: ParserType
                   }
  deriving (Show, Read, Eq)

instance FromJSON ElementaryParser where
  parseJSON (Object o) =
    do parserType <- o .: "type"
       case parserType of
          String "oneOf"      ->
            ElementaryParser <$> o.: "name" <*> o .: "options" <*> (fmap OneOf (o .: "values"))

          String "time"       ->
            ElementaryParser <$> o.: "name" <*> o .: "options" <*> (fmap Time (o .: "pattern"))

          String "date"       ->
            ElementaryParser <$> o.: "name" <*> o .: "options" <*> (fmap Date (o .: "pattern"))

          String "characters" ->
            ElementaryParser <$> o.: "name" <*> o .: "options" <*> (fmap Characters (o .: "value"))

          String "matchUntilIncluded" ->
            ElementaryParser <$> o.: "name" <*> o .: "options" <*> (fmap MatchUntilIncluded (o .: "value"))

          String "matchUntilExcluded" ->
            ElementaryParser <$> o.: "name" <*> o .: "options" <*> (fmap MatchUntilExcluded (o .: "value"))

          String "matchFor" ->
            ElementaryParser <$> o.: "name" <*> o .: "options" <*> (fmap MatchFor (o .: "count"))

          String "matchUntilEnd" ->
            ElementaryParser <$> pure "matchUntilEnd" <*> o .: "options" <*> pure MatchUntilEnd

          _                   ->
            ElementaryParser <$> o.: "name" <*> o .: "options" <*> (fmap InvalidParser (o .: "type"))


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

instance FromJSON ParsingOptions where
  parseJSON (Object o) = do
    keepResult <- o .: "keepResult"

    return $ ParsingOptions { keepResult = keepResult
                            }


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
  NamedElementaryParser { name :: String
                        , parser :: ElementaryParser
                        }
  deriving (Show, Read, Eq)

instance FromJSON NamedElementaryParser where
  parseJSON (Object o) =
    NamedElementaryParser <$> o .: "name" <*> o .: "parser"


data ElementaryParsingRequest =
  ElementaryParsingRequest { target :: String
                           , parser :: ElementaryParser
                           }
  deriving (Show, Read, Eq)

instance FromJSON ElementaryParsingRequest where
  parseJSON (Object o) =
    ElementaryParsingRequest <$> o .: "target" <*> o .: "parser"


data ElementaryParsingResponse =
  ElementaryParsingResponse { name :: String
                            , result :: ParsingResultType
                            }
  deriving (Show, Read, Eq)

instance ToJSON ElementaryParsingResponse where
  toJSON (ElementaryParsingResponse name (OneOfResult result))               = object [ "name" .= name, "result" .= result ]
  toJSON (ElementaryParsingResponse name (TimeResult result))                = object [ "name" .= name, "result" .= result ]
  toJSON (ElementaryParsingResponse name (DateResult result))                = object [ "name" .= name, "result" .= result ]
  toJSON (ElementaryParsingResponse name (CharactersResult result))          = object [ "name" .= name, "result" .= result ]
  toJSON (ElementaryParsingResponse name (MatchUntilIncludedResult result))  = object [ "name" .= name, "result" .= result ]
  toJSON (ElementaryParsingResponse name (MatchUntilExcludedResult result))  = object [ "name" .= name, "result" .= result ]
  toJSON (ElementaryParsingResponse name (MatchForResult result))            = object [ "name" .= name, "result" .= result ]
  toJSON (ElementaryParsingResponse name (MatchUntilEndResult result))       = object [ "name" .= name, "result" .= result ]
  toJSON (ElementaryParsingResponse name (ParsingError result))              = object [ "name" .= name, "error" .= result  ]


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



-- Logfile Parser Models


type CreateLogfileParserRequest = LogfileParser


data LogfileParser =
  LogfileParser { name :: String
                , namedParsers :: [ NamedElementaryParser ]
                }
  deriving (Show, Read, Eq)

instance FromJSON LogfileParser where
  parseJSON (Object o) =
    LogfileParser <$> o .: "name" <*> o .: "parsers"


data LogfileParsingRequest =
  LogfileParsingRequest { target :: String
                        , parser :: LogfileParser
                        }

instance FromJSON LogfileParsingRequest where
  parseJSON (Object o) =
    LogfileParsingRequest <$> o .: "target" <*> o .: "parser"


data LogfileParsingFileRequest =
  LogfileParsingFileRequest { name :: String
                            , logfile :: FilePath
                            }
  deriving (Show)

instance FromMultipart Tmp LogfileParsingFileRequest where
  fromMultipart form =
      LogfileParsingFileRequest
          <$> fmap T.unpack (lookupInput "name" form)
          <*> fmap fdPayload (lookupFile "logfile" form)


data LogfileParsingResponse =
    LogfileParsingResponse [[ElementaryParsingResponse]]
  | LogfileParsingError String
  deriving (Eq, Show)

instance ToJSON LogfileParsingResponse where
  toJSON (LogfileParsingResponse val) = object [ "result" .= val ]
  toJSON (LogfileParsingError err) = object [ "error" .= err ]
