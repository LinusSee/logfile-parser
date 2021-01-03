module ValidationOrchestration
( validateParsingRequest

) where

import Data.Either (isRight)

import qualified Validation as Validation
import ValidationModels (ValidationError)
import HttpErrors (Problem (..))
import CustomParsers


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
