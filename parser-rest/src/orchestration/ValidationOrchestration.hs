module ValidationOrchestration
( validateCreateLogfileParserRequest
, validateLogfileParsingUrlRequest
, validateLogfileParsingRequest
, validateElementaryParserToCreate
, validateParsingRequest
, validateLogfileParsingFileRequest
, validateParsingUrlRequest
) where

import Data.Either (isRight, fromLeft)

import qualified Validation as Validation
import ValidationModels (ValidationError (..), ValidationType (..))
import HttpErrors (Problem (..))

import qualified RestParserModels as RM




validateCreateLogfileParserRequest :: RM.CreateLogfileParserRequest -> Either Problem RM.CreateLogfileParserRequest
validateCreateLogfileParserRequest request@(RM.LogfileParser name parsers) =
    case isValidRequest of
      True ->
        Right request

      False ->
        Left $ errorsToValidationProblem (fromLeft [] validatedParser)

    where validatedParser = Validation.validateLogfileParser request
          isValidRequest = isRight validatedParser


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


validateLogfileParsingRequest :: RM.LogfileParsingRequest -> Either Problem RM.LogfileParsingRequest
validateLogfileParsingRequest request@(RM.LogfileParsingRequest target (RM.LogfileParser name parsers)) =
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

        logfileParser = RM.LogfileParser name parsers


validateLogfileParsingFileRequest :: RM.LogfileParsingFileRequest -> Either Problem RM.LogfileParsingFileRequest
validateLogfileParsingFileRequest request@(RM.LogfileParsingFileRequest parserName logfile) =
  case isValidRequest of
    True ->
      Right request

    False ->
      Left $ errorsToValidationProblem $
        Validation.appendError validatedName []

  where validatedName = Validation.validateLogfileParserExists parserName
        isValidRequest = isRight validatedName



validateElementaryParserToCreate :: RM.ElementaryParser -> Either Problem RM.ElementaryParser
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



validateParsingRequest :: RM.ElementaryParsingRequest -> Either Problem RM.ElementaryParsingRequest
validateParsingRequest request@(RM.ElementaryParsingRequest target parser) =
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
