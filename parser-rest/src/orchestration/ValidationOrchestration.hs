module ValidationOrchestration
( validateCreateLogfileParserRequest
, validateLogfileParsingUrlRequest
, validateLogfileParsingRequest
, validateElementaryParserToCreate
, validateParsingRequest
, validateParsingUrlRequest
) where

import Data.Either (isRight, fromLeft)

import qualified Validation as Validation
import ValidationModels (ValidationError (..), ValidationType (..))
import HttpErrors (Problem (..))
import CustomParsers




validateCreateLogfileParserRequest :: CreateLogfileParserRequest -> Either Problem CreateLogfileParserRequest
validateCreateLogfileParserRequest request@(CreateLogfileParserRequest name parsers) =
    case isValidRequest of
      True ->
        Right request

      False ->
        Left $ errorsToValidationProblem (fromLeft [] validatedParser)

    where validatedParser = Validation.validateLogfileParser logfileParser
          isValidRequest = isRight validatedParser

          logfileParser = LogfileParser name parsers


validateLogfileParsingUrlRequest :: String -> Maybe String -> Either Problem (String, String)
validateLogfileParsingUrlRequest parserName maybeTarget =
    case maybeTarget of
      Just target ->
        case isValidRequest of
          True ->
            Right (parserName, target)

          False ->
            Left $ errorsToValidationProblem $
                ( Validation.appendError validatedName
                . Validation.appendError validatedTarget
                ) []

        where validatedName = Validation.validateLogfileParserExists parserName
              validatedTarget = Validation.validateTarget target
              isValidRequest = isRight validatedName && isRight validatedTarget

      Nothing ->
        Left $ errorsToValidationProblem
            [ValidationError (QueryParamValidation "target") "Target value must be present."]


validateLogfileParsingRequest :: LogfileParsingRequest -> Either Problem LogfileParsingRequest
validateLogfileParsingRequest request@(LogfileParsingRequest target (CreateLogfileParserRequest name parsers)) =
  case isValidRequest of
    True ->
      Right request

    False ->
      Left $ errorsToValidationProblem $
          ( Validation.appendError validatedTarget
          ) (fromLeft [] validatedParser)

  where validatedTarget = Validation.validateTarget target
        validatedParser = Validation.validateLogfileParser logfileParser
        isValidRequest = isRight validatedTarget && isRight validatedParser

        logfileParser = LogfileParser name parsers



validateElementaryParserToCreate :: ElementaryParser -> Either Problem ElementaryParser
validateElementaryParserToCreate parser =
  case isValidParser of
    True ->
      Right parser

    False ->
      Left $ errorsToValidationProblem $ fromLeft [] validatedParser

  where validatedParser = Validation.validateElementaryParser parser
        isValidParser = isRight validatedParser


validateParsingUrlRequest :: String -> Maybe String -> Either Problem (String, String)
validateParsingUrlRequest parserName maybeTarget =
  case maybeTarget of
    Just target ->
      case isValidRequest of
        True ->
          Right (parserName, target)

        False ->
          Left $ errorsToValidationProblem $
              ( Validation.appendError validatedTarget
              . Validation.appendError validatedParserName
              ) []

      where validatedTarget = Validation.validateTarget target
            validatedParserName = Validation.validateElementaryParserExists parserName

            isValidRequest = isRight validatedTarget && isRight validatedParserName

    Nothing ->
      Left $ errorsToValidationProblem
          [ValidationError (QueryParamValidation "target") "Target value must be present."]



validateParsingRequest :: ParsingRequest -> Either Problem ParsingRequest
validateParsingRequest request@(ParsingRequest target parser) =
  case isValidRequest of
    True ->
      Right request

    False ->
      Left $ errorsToValidationProblem $
        ( Validation.appendError validatedTarget
        ) (fromLeft [] validatedParser)

  where validatedTarget = Validation.validateTarget target
        validatedParser = Validation.validateElementaryParser parser

        isValidRequest = isRight validatedTarget && isRight validatedParser



errorsToValidationProblem :: [ValidationError] -> Problem
errorsToValidationProblem errs = Problem
  { problemType = "http://localhost/problems/validation-error"
  , title = "Failed to validate request parameters."
  , status = 400
  , detail = ""
  , errors = errs
  }
