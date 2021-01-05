module Validation
( Valid
, fromValid
, validateTarget
, validateElementaryParser
, validateElementaryParserExists
, validateLogfileParser
, validateLogfileParserExists
, appendError
) where

import Data.Either (isRight)

import ValidationModels
  ( ValidationError (..)
  , ValidationType (..)
  )
import CustomParsers
  ( LogfileParser (..)
  , ElementaryParser
  )



data Valid a = Valid a

fromValid :: Valid a -> a
fromValid (Valid val) = val


validateTarget :: String -> Either ValidationError (Valid String)
validateTarget target =
  case targetIsValid of
    True ->
      Right $ Valid target

    False ->
      Left $ ValidationError
          (FieldValidation "target")
          "The target string to parse must not be empty."

  where targetIsValid = not $ null target


validateElementaryParserExists :: String -> Either ValidationError (Valid String)
validateElementaryParserExists name = Right $ Valid name


validateLogfileParserExists :: String -> Either ValidationError (Valid String)
validateLogfileParserExists name = Right $ Valid name


validateElementaryParser :: ElementaryParser -> Either ValidationError (Valid ElementaryParser)
validateElementaryParser parser = Right $ Valid parser


validateLogfileParser :: LogfileParser -> Either [ValidationError] (Valid LogfileParser)
validateLogfileParser parser@(LogfileParser name parsers) =
  case logfileParserIsValid of
    True ->
      Right $ Valid parser

    False ->
      Left $
        ( appendError validatedName
        . appendError validatedParsers) []

  where validatedName = validateParserName name
        validatedParsers = validateParsersList parsers
        logfileParserIsValid = isRight validatedName && isRight validatedParsers


validateParserName :: String -> Either ValidationError (Valid String)
validateParserName name =
  case nameIsValid of
    True ->
      Right $ Valid name

    False ->
      Left $ ValidationError
          (FieldValidation "name")
          "The name of the parser must not be empty."

  where nameIsValid = not $ null name


validateParsersList :: [(String, ElementaryParser)] -> Either ValidationError (Valid [(String, ElementaryParser)])
validateParsersList parsers =
  case parsersIsValid of
    True ->
      Right $ Valid parsers

    False ->
      Left $ ValidationError
          (FieldValidation "parsers")
          "A logfile parsers must contain at least one elementary parser."

  where parsersIsValid = not $ null parsers


appendError :: Either ValidationError (Valid a) -> [ValidationError] -> [ValidationError]
appendError (Left err) errs = err : errs
appendError (Right _) errs = errs
