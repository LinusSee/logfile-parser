module ValidationOrchestration
( validateParsingRequest
, validateParsingUrlRequest
) where

import Data.Either (isRight)

import qualified Validation as Validation
import ValidationModels (ValidationError (..), ValidationType (..))
import HttpErrors (Problem (..))
import CustomParsers




validateElementaryParserToCreate :: ElementaryParser -> Either Problem ElementaryParser
validateElementaryParserToCreate parser =
  case isValidParser of
    True ->
      Right parser

    False ->
      Left $ errorsToValidationProblem $ Validation.appendError validatedParser []

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
        . Validation.appendError validatedParser) []

  where validatedTarget = Validation.validateTarget target
        validatedParser = Validation.validateElementaryParser parser

        isValidRequest = isRight validatedTarget && isRight validatedParser


-- validateCreateLogfileParserRequest :: CreateLogfileParserRequest -> Either [ValidationError] (Validation.Valid LogfileParser)
-- validateCreateLogfileParserRequest (CreateLogfileParserRequest target parsers) =
--   case requestIsValid of
--     Right
--
--     Left
--
--   where validatedTarget = Validation.validateTarget target
--         validatedParser = Validation.validateLogfileParser logfileParser
--         requestIsValid = isRight validatedTarget && isRight validatedParser
--
--         logfileParser = LogfileParser name mappedParsers
--         mapParser ( NamedParser name parser ) = (name, parser)
--         mappedParsers = map mapParser parsers


errorsToValidationProblem :: [ValidationError] -> Problem
errorsToValidationProblem errs = Problem
  { problemType = "http://localhost/problems/validation-error"
  , title = "Failed to validate request parameters."
  , status = 400
  , detail = ""
  , errors = errs
  }
