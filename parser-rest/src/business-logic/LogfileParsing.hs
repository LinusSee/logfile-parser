{-# LANGUAGE FlexibleContexts #-}


module LogfileParsing
( applyLogfileParser
) where

import qualified Text.Parsec as Parsec
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Data.List
import Data.Time
import CustomParsers
  ( ElementaryParser (..)
  , LogfileParser (..)
  , ParsingRequest (..)
  , ParsingResponse (..)
  , LogfileParsingRequest (..)
  , LogfileParsingResponse (..)
  )

import ElementaryParsing as ElementaryParsing


parse rule text = Parsec.parse rule "Logfile parser (source name)" text




applyLogfileParser :: String -> LogfileParser -> Either Parsec.ParseError LogfileParsingResponse
applyLogfileParser target (LogfileParser name parsers) =
  parse (applyListOfParsers parsers) target


applyListOfParsers :: [ElementaryParser] -> Parsec.Parsec String () LogfileParsingResponse
applyListOfParsers parsers = do
  result <- runThroughList parsers
  return $ LogfileParsingResponse result


runThroughList :: [ ElementaryParser ] -> Parsec.Parsec String () [ParsingResponse]
runThroughList [] = do
  return []
runThroughList (x:xs) = do
  result <- ElementaryParsing.chooseParser x
  next <- runThroughList xs
  return (result : next)
