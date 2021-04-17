{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

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
, LogfileParsingFileRequest (..)
, LogfileParsingResponse (..)
, CreateLogfileParserRequest (..)
, NamedElementaryParser (..)
, ParsingOptions (..)
, ParsingOption (..)
, fromDbElementaryParser
) where

import Data.Aeson
import Data.Aeson(Value(Object))
import Data.Aeson.TH
import qualified Data.HashMap.Lazy as HML
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (TimeOfDay, Day)
import Servant.Multipart

import qualified DbParserModels as DM



-- MODELS
data ElementaryParser =
  ElementaryParser String ParsingOptions BasicParser
  deriving (Show, Read, Eq)

data ParsingOptions =
  ParsingOptions [ParsingOption]
  deriving (Show, Read, Eq)

data ParsingOption =
  KeepResult Bool
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


instance ToJSON ParsingOption where
  toJSON (KeepResult bool) = object [ "keepResult" .= bool ]

instance ToJSON ParsingOptions where
  toJSON (ParsingOptions xs) = mergeOptions (map toJSON xs)

    where mergeOptions = (Object . HML.unions . map (\(Object x) -> x))


instance ToJSON ElementaryParser where
  -- toJSON (ElementaryParser n (OneOf xs))             = object [ "type" .= ("oneOf" :: Text),               "name" .= n, "values" .= xs ]
  -- toJSON (ElementaryParser n (Time p))               = object [ "type" .= ("time" :: Text),                "name" .= n, "pattern" .= p ]
  -- toJSON (ElementaryParser n (Date p))               = object [ "type" .= ("date" :: Text),                "name" .= n, "pattern" .= p ]
  -- toJSON (ElementaryParser n (Characters s))         = object [ "type" .= ("characters" :: Text),          "name" .= n, "value" .= s ]
  -- toJSON (ElementaryParser n (MatchUntilIncluded s)) = object [ "type" .= ("matchUntilIncluded" :: Text),  "name" .= n, "value" .= s ]
  -- toJSON (ElementaryParser n (MatchUntilExcluded s)) = object [ "type" .= ("matchUntilExcluded" :: Text),  "name" .= n, "value" .= s ]
  -- toJSON (ElementaryParser n (MatchFor i))           = object [ "type" .= ("matchFor" :: Text),            "name" .= n, "count" .= i ]
  -- toJSON (ElementaryParser n MatchUntilEnd)          = object [ "type" .= ("matchUntilEnd" :: Text),       "name" .= ("matchUntilEnd" :: Text) ]

  toJSON (ElementaryParser n os (OneOf xs))             = object [ "type" .= ("oneOf" :: Text),               "name" .= n, "options" .= os, "values" .= xs ]
  toJSON (ElementaryParser n os (Time p))               = object [ "type" .= ("time" :: Text),                "name" .= n, "options" .= os, "pattern" .= p ]
  toJSON (ElementaryParser n os (Date p))               = object [ "type" .= ("date" :: Text),                "name" .= n, "options" .= os, "pattern" .= p ]
  toJSON (ElementaryParser n os (Characters s))         = object [ "type" .= ("characters" :: Text),          "name" .= n, "options" .= os, "value" .= s ]
  toJSON (ElementaryParser n os (MatchUntilIncluded s)) = object [ "type" .= ("matchUntilIncluded" :: Text),  "name" .= n, "options" .= os, "value" .= s ]
  toJSON (ElementaryParser n os (MatchUntilExcluded s)) = object [ "type" .= ("matchUntilExcluded" :: Text),  "name" .= n, "options" .= os, "value" .= s ]
  toJSON (ElementaryParser n os (MatchFor i))           = object [ "type" .= ("matchFor" :: Text),            "name" .= n, "options" .= os, "count" .= i ]
  toJSON (ElementaryParser n os MatchUntilEnd)          = object [ "type" .= ("matchUntilEnd" :: Text),       "name" .= ("matchUntilEnd" :: Text), "options" .= os ]

instance FromJSON ParsingOptions where
  parseJSON (Object o) = do
    keepResult <- fmap KeepResult (o .: "keepResult")

    return $ ParsingOptions [ keepResult
                            ]


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
            -- TODO: Simple return should suffice
            ElementaryParser <$> pure "matchUntilEnd" <*> o .: "options" <*> pure MatchUntilEnd
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
    LogfileParsingResponse [[ParsingResponse]]
  | LogfileParsingError String
  deriving (Eq, Show)

instance ToJSON LogfileParsingResponse where
  toJSON (LogfileParsingResponse val) = object [ "result" .= val ]
  toJSON (LogfileParsingError err) = object [ "error" .= err ]



fromDbElementaryParser :: DM.ElementaryParser -> ElementaryParser
fromDbElementaryParser DM.ElementaryParser{DM.name, DM.options, DM.parserType} =
  ElementaryParser
          name
          (fromDbParsingOptions options)
          (fromDbParserType parserType)


fromDbParsingOptions :: DM.ParsingOptions -> ParsingOptions
fromDbParsingOptions DM.ParsingOptions{DM.keepResult} =
  ParsingOptions [KeepResult keepResult]

fromDbParserType :: DM.ParserType -> BasicParser
fromDbParserType parserType =
  case parserType of
    DM.OneOf xs             -> OneOf xs
    DM.Time pattern         -> Time pattern
    DM.Date pattern         -> Date pattern
    DM.Characters x         -> Characters x
    DM.MatchUntilIncluded x -> MatchUntilIncluded x
    DM.MatchUntilExcluded x -> MatchUntilExcluded x
    DM.MatchFor c           -> MatchFor c
    DM.MatchUntilEnd        -> MatchUntilEnd


-- toDbElementaryParser :: ElementaryParser -> DM.ElementaryParser
-- toDbElementaryParser
