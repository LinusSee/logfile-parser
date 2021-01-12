module Validation
( Valid
, fromValid
, validateTarget
, validateParserName
, validateElementaryParser
, validateElementaryParserExists
, validateLogfileParser
, validateLogfileParserExists
, appendError
) where

import Data.Either (fromRight, fromLeft, isRight, isLeft)
import qualified Data.Char as Char

import ValidationModels
  ( ValidationError (..)
  , ValidationType (..)
  )
import CustomParsers
  ( LogfileParser (..)
  , ElementaryParser (..)
  )



data Valid a = Valid a
  deriving (Show, Eq)

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
      case emptyParserList of
        True ->
          Left $
            ValidationError
                (FieldValidation "parsers")
                "A logfile parsers must contain at least one elementary parser."

        False ->
          Left $
            ValidationError
                (FieldValidation "parsers")
                "All parsers in the list must be valid."

  where parsersIsValid = not emptyParserList && not (True `elem` (map isLeft validatedParsers))
        emptyParserList = null parsers
        validatedParsers = map validateNamedParser parsers


validateNamedParser :: (String, ElementaryParser) -> Either [ValidationError] (Valid (String, ElementaryParser))
validateNamedParser (name, parser) =
  case isValid of
    True ->
      Right $ Valid (name, parser)

    False ->
      Left $ appendError validatedName (fromLeft [] validatedParser)

  where validatedName = validateParserName name
        validatedParser = validateElementaryParser parser
        isValid = isRight validatedName && isRight validatedParser



appendError :: Either ValidationError (Valid a) -> [ValidationError] -> [ValidationError]
appendError (Left err) errs = err : errs
appendError (Right _) errs = errs


extractErrors :: [Either ValidationError (Valid a)] -> [ValidationError]
extractErrors [] = []
extractErrors (x:xs) =
    case x of
      Right _ ->
        extractErrors xs
      Left err ->
        err : extractErrors xs



validateOneOfValues :: [String] -> Either ValidationError (Valid [String])
validateOneOfValues values =
  case valuesIsValid of
    True ->
      Right $ Valid values

    False ->
      Left $ ValidationError
        (FieldValidation "values")
        "A oneOf parser must contain at least one nonempty value/string to match."

  where valuesIsValid =    (not $ null values)
                        && (not $ True `elem` (map null values))


validateTimePattern :: String -> Either ValidationError (Valid String)
validateTimePattern pattern =
  case patternIsValid of
    True ->
      Right $ Valid pattern

    False ->
      Left $ ValidationError
        (FieldValidation "pattern")
        ( "A time parser must match the following format: Two blocks of 'HH' and 'MM'"
        ++ " separated by a single char. The order of the blocks does not matter (e.g. HH-MM).")

  where patternIsValid = "HH" `elem` comb && "MM" `elem` comb && length pattern == 5
        patternUpper = map Char.toUpper pattern
        comb = [take 2 patternUpper] ++ [drop 3 patternUpper]


validateDatePattern :: String -> Either ValidationError (Valid String)
validateDatePattern pattern =
  case patternIsValid of
    True ->
      Right $ Valid pattern

    False ->
      Left $ ValidationError
        (FieldValidation "pattern")
        ( "A time parser must match the following format: Three blocks of 'YYYY', 'MM', and 'DD'"
        ++ " separated by a single char. The order of the blocks does not matter (e.g. MM-YYYY.DD).")

  where patternIsValid =     ("YYYY" `elem` comb1 && "MM" `elem` comb1 && "DD" `elem` comb1)
                          || ("YYYY" `elem` comb2 && "MM" `elem` comb2 && "DD" `elem` comb2)
                          || ("YYYY" `elem` comb3 && "MM" `elem` comb3 && "DD" `elem` comb3)
                          && length pattern == 10
        patternUpper = map Char.toUpper pattern
        comb1 = [take 4 patternUpper] ++ [take 2 (drop 5 patternUpper)] ++ [drop 8 patternUpper]
        comb2 = [take 2 patternUpper] ++ [take 4 (drop 3 patternUpper)] ++ [drop 8 patternUpper]
        comb3 = [take 2 patternUpper] ++ [take 2 (drop 3 patternUpper)] ++ [drop 6 patternUpper]


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
