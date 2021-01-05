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
  , ElementaryParser (..)
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


validateElementaryParser :: ElementaryParser -> Either [ValidationError] (Valid ElementaryParser)
validateElementaryParser parser =
    case parser of
      OneOf name values ->
          let validatedName = validateParserName name
              validatedValues = validateOneOfValues values
          in  gatherResult validatedName validatedValues

      Time name pattern ->
          let validatedName = validateParserName name
              validatedPattern = validateTimePattern pattern
          in  gatherResult validatedName validatedPattern

      Date name pattern ->
          let validatedName = validateParserName name
              validatedPattern = validateDatePattern pattern
          in  gatherResult validatedName validatedPattern

      Characters name value ->
          let validatedName = validateParserName name
              validatedValue = validateCharactersValue value
          in  gatherResult validatedName validatedValue

      MatchUntilIncluded name value ->
          let validatedName = validateParserName name
              validatedValue = validateMatchUntilIncludedValue value
          in  gatherResult validatedName validatedValue

      MatchUntilExcluded name value ->
          let validatedName = validateParserName name
              validatedValue = validateMatchUntilExcludedValue value
          in  gatherResult validatedName validatedValue

      MatchFor name count ->
          let validatedName = validateParserName name
              validatedCount = validateMatchForCount count
          in  gatherResult validatedName validatedCount

      MatchUntilEnd _ ->
          Right $ Valid parser

    where gatherResult r1 r2 =
            let isValidResult = isRight r1 && isRight r2
                errs = (appendError r1 . appendError r2) []
            in case isValidResult of
                True ->
                  Right $ Valid parser

                False ->
                  Left errs




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
  -- TODO: Validate the parsers themselves, not only the names
  where parsersIsValid = not $ null parsers


appendError :: Either ValidationError (Valid a) -> [ValidationError] -> [ValidationError]
appendError (Left err) errs = err : errs
appendError (Right _) errs = errs



validateOneOfValues :: [String] -> Either ValidationError (Valid [String])
validateOneOfValues values =
  case valuesIsValid of
    True ->
      Right $ Valid values

    False ->
      Left $ ValidationError
        (FieldValidation "values")
        "A oneOf parser must contain at least one value to match."

  where valuesIsValid = not $ null values
  -- TODO: Empty elements are invalid


validateTimePattern :: String -> Either ValidationError (Valid String)
validateTimePattern pattern =
  case patternIsValid of
    True ->
      Right $ Valid pattern

    False ->
      Left $ ValidationError
        (FieldValidation "pattern")
        ( "A time parser must match the following format: Two blocks of 'HH' and 'MM'"
        ++ " separated by a single char. The order of the blocks does not matter.")

  where patternIsValid = not $ null pattern -- TODO: Check for actual pattern


validateDatePattern :: String -> Either ValidationError (Valid String)
validateDatePattern pattern =
  case patternIsValid of
    True ->
      Right $ Valid pattern

    False ->
      Left $ ValidationError
        (FieldValidation "pattern")
        ( "A time parser must match the following format: Three blocks of 'YYYY', 'MM', and 'DD'"
        ++ " separated by a single char. The order of the blocks does not matter.")

  where patternIsValid = not $ null pattern -- TODO: Check for actual pattern


validateCharactersValue :: String -> Either ValidationError (Valid String)
validateCharactersValue value =
  case valueIsValid of
    True ->
      Right $ Valid value

    False ->
      Left $ ValidationError
        (FieldValidation "value") "A characters parser must contain at least a single char to match."

  where valueIsValid = not $ null value


validateMatchUntilIncludedValue :: String -> Either ValidationError (Valid String)
validateMatchUntilIncludedValue value =
  case valueIsValid of
    True ->
      Right $ Valid value

    False ->
      Left $ ValidationError
        (FieldValidation "value") "A matchUntilIncluded parser must contain at least a single char to match."

  where valueIsValid = not $ null value


validateMatchUntilExcludedValue :: String -> Either ValidationError (Valid String)
validateMatchUntilExcludedValue value =
  case valueIsValid of
    True ->
      Right $ Valid value

    False ->
      Left $ ValidationError
        (FieldValidation "value") "A matchUntilExcluded parser must contain at least a single char to match."

  where valueIsValid = not $ null value


validateMatchForCount :: Int -> Either ValidationError (Valid Int)
validateMatchForCount count =
  case countIsValid of
    True ->
      Right $ Valid count

    False ->
      Left $ ValidationError
        (FieldValidation "count") "A matchFor parser must have a count greater 0."

  where countIsValid = count > 0
