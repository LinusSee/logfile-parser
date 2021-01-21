module ValidationSpec
( spec
) where

import qualified Data.Either as Either
import qualified Data.Char as Char
import Data.List (isInfixOf)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Validation
import ValidationModels
import CustomParsers


spec :: Spec
spec = do
    describe "validateTarget" $ do
      context "when provided with a valid input (nonempty string)" $ do
          it "returns a <Valid> type containing the target string if it is not null" $ do
              fromRight' (validateTarget "a valid string") `shouldBe` Just "a valid string"



      context "when provided with an invalid input (empty string)" $ do
          it "returns a correct <ValidationError> if target string is empty" $ do
              validateTarget "" `shouldBe` (Left $ ValidationError
                                              (FieldValidation "target")
                                              "The target string to parse must not be empty.")



      context "properties" $ do
          prop "returns a <ValidationError> for target == null and <Valid> containing the target otherwise" $
            \target -> if null target
                       then validateTarget target `shouldBe` (Left $ ValidationError
                                                       (FieldValidation "target")
                                                       "The target string to parse must not be empty.")
                       else fromRight' (validateTarget target) `shouldBe` Just target




    describe "validateParserName" $ do
      context "when provided with a valid input (nonempty string)" $ do
          it "returns a <Valid> type containing the parserName string" $ do
              fromRight' (validateParserName "a valid name") `shouldBe` Just "a valid name"



      context "when provided with an invalid input (empty string)" $ do
          it "returns a correct <ValidationError>" $ do
              validateParserName "" `shouldBe` (Left $ ValidationError
                                                  (FieldValidation "name")
                                                  "The name of the parser must not be empty.")



      context "properties" $ do
          prop "returns a <ValidationError> for parserName == null and <Valid> containing the parserName otherwise" $
            \parserName -> if null parserName
                       then validateParserName parserName `shouldBe` (Left $ ValidationError
                                                       (FieldValidation "name")
                                                       "The name of the parser must not be empty.")
                       else fromRight' (validateParserName parserName) `shouldBe` Just parserName




    describe "validateElementaryParser" $ do
      context "when provided with a valid OneOf ElementaryParser" $ do
        it "returns a <Valid> type containing the parser" $ do
            let parser = OneOf "some parser name" ["elem1", "elem2"]
            fromRight' (validateElementaryParser parser) `shouldBe` Just parser

      context "when provided with an invalid input (empty list)" $ do
        it "returns a correct <ValidationError> list" $ do
            let parser = OneOf "some parser name" []
            validateElementaryParser parser `shouldBe` Left [ ValidationError
                                                            (FieldValidation "values")
                                                            "A oneOf parser must contain at least one nonempty value/string to match."
                                                            ]

      context "properties" $ do
        prop "returns a <ValidationError> list if invalid and a <Valid> containing the parser otherwise" $
          \parserName elements -> do
              let parser = OneOf parserName elements
              let validationResult = validateElementaryParser parser

              if null parserName && (null elements || (not . null) (filter null elements))
              then validationResult `shouldBe` Left [
                                                ValidationError
                                                  (FieldValidation "name")
                                                  "The name of the parser must not be empty.",
                                                ValidationError
                                                  (FieldValidation "values")
                                                  "A oneOf parser must contain at least one nonempty value/string to match."
                                                ]
              else
                if null parserName
                then validationResult `shouldBe` Left[ ValidationError
                                                        (FieldValidation "name")
                                                        "The name of the parser must not be empty."]
                else
                  if null elements || (not . null) (filter null elements)
                  then validationResult `shouldBe` Left[ ValidationError
                                                          (FieldValidation "values")
                                                          "A oneOf parser must contain at least one nonempty value/string to match."]
                  else fromRight' (validateElementaryParser parser) `shouldBe` Just parser




      context "when provided with a valid Time ElementaryParser" $ do
        it "returns a <Valid> type containing the parser" $ do
          let parser = Time "some parser name" "HH.MM"
          fromRight' (validateElementaryParser parser) `shouldBe` Just parser

      context "when provided with an invalid input (invalid pattern)" $ do
        it "returns a correct <ValidationError> list" $ do
            let parser = Time "some parser name" ".HHMM"
            validateElementaryParser parser `shouldBe` Left [ ValidationError
                                                                (FieldValidation "pattern")
                                                                ( "A time parser must match the following format: Two blocks of 'HH' and 'MM'"
                                                                ++ " separated by a single char. The order of the blocks does not matter (e.g. HH-MM).")
                                                            ]

      context "properties" $ do
        prop "returns a <ValidationError> list if invalid and a <Valid> containing the parser otherwise" $
          \parserName pattern -> do
              let upperPattern = map Char.toUpper pattern
              let parser = Time parserName pattern
              let validationResult = validateElementaryParser parser

              -- The second part of the condition is incorrect: Leave this until I know how to generate proper testdata
              if null parserName && (length upperPattern /= 5 || not ("HH" `isInfixOf` upperPattern) || not ("MM" `isInfixOf` upperPattern))
              then validationResult `shouldBe` Left [
                                                ValidationError
                                                  (FieldValidation "name")
                                                  "The name of the parser must not be empty.",
                                                ValidationError
                                                  (FieldValidation "pattern")
                                                  ( "A time parser must match the following format: Two blocks of 'HH' and 'MM'"
                                                  ++ " separated by a single char. The order of the blocks does not matter (e.g. HH-MM).")
                                                ]
              else
                if null parserName
                then validationResult `shouldBe` Left[ ValidationError
                                                        (FieldValidation "name")
                                                        "The name of the parser must not be empty."]
                else
                  if (length upperPattern /= 5 || not ("HH" `isInfixOf` upperPattern) || not ("MM" `isInfixOf` upperPattern))
                  then validationResult `shouldBe` Left[ ValidationError
                                                          (FieldValidation "pattern")
                                                          ( "A time parser must match the following format: Two blocks of 'HH' and 'MM'"
                                                          ++ " separated by a single char. The order of the blocks does not matter (e.g. HH-MM).")]
                  else fromRight' (validateElementaryParser parser) `shouldBe` Just parser




      context "when provided with a valid Date ElementaryParser" $ do
        it "returns a <Valid> type containing the parser" $ do
          let parser = Date "some parser name" "YYYY-DD-MM"
          fromRight' (validateElementaryParser parser) `shouldBe` Just parser

      context "when provided with an invalid input (invalid pattern)" $ do
        it "returns a correct <ValidationError> list" $ do
            let parser = Date "some parser name" "YYYY.MMDD."
            validateElementaryParser parser `shouldBe` Left [ ValidationError
                                                              (FieldValidation "pattern")
                                                              ( "A time parser must match the following format: Three blocks of 'YYYY', 'MM', and 'DD'"
                                                              ++ " separated by a single char. The order of the blocks does not matter (e.g. MM-YYYY.DD).")
                                                            ]

      context "properties" $ do
        prop "returns a <ValidationError> list if invalid and a <Valid> containing the parser otherwise" $
          \parserName pattern -> do
              let upperPattern = map Char.toUpper pattern
              let parser = Date parserName pattern
              let validationResult = validateElementaryParser parser

              -- The second part of the condition is incorrect: Leave this until I know how to generate proper testdata
              if null parserName && ( length upperPattern /= 10 || not ("YYYY" `isInfixOf` upperPattern)
                                    || not ("MM" `isInfixOf` upperPattern) || not ("DD" `isInfixOf` upperPattern))
              then validationResult `shouldBe` Left [
                                                ValidationError
                                                  (FieldValidation "name")
                                                  "The name of the parser must not be empty.",
                                                ValidationError
                                                  (FieldValidation "pattern")
                                                  ( "A time parser must match the following format: Three blocks of 'YYYY', 'MM', and 'DD'"
                                                  ++ " separated by a single char. The order of the blocks does not matter (e.g. MM-YYYY.DD).")
                                                ]
              else
                if null parserName
                then validationResult `shouldBe` Left [ ValidationError
                                                        (FieldValidation "name")
                                                        "The name of the parser must not be empty."]
                else
                  if length upperPattern /= 10 || not ("YYYY" `isInfixOf` upperPattern) || not ("MM" `isInfixOf` upperPattern) || not ("DD" `isInfixOf` upperPattern)
                  then validationResult `shouldBe` Left [ ValidationError
                                                          (FieldValidation "pattern")
                                                          ( "A time parser must match the following format: Three blocks of 'YYYY', 'MM', and 'DD'"
                                                          ++ " separated by a single char. The order of the blocks does not matter (e.g. MM-YYYY.DD).")]
                  else fromRight' (validateElementaryParser parser) `shouldBe` Just parser



      context "when provided with a valid Characters ElementaryParser" $ do
        it "returns a <Valid> type containing the parser" $ do
          let parser = Characters "some parser name" "stringToMatch"
          fromRight' (validateElementaryParser parser) `shouldBe` Just parser

      context "when provided with an invalid input (empty string)" $ do
        it "returns a correct <ValidationError> list" $ do
            let parser = Characters "some parser name" ""
            validateElementaryParser parser `shouldBe` Left [ ValidationError
                                                                (FieldValidation "value")
                                                                "A characters parser must contain at least a single char to match."
                                                            ]

      context "properties" $ do
        prop "returns a <ValidationError> list if invalid and a <Valid> containing the parser otherwise" $
          \parserName value -> do
              let parser = Characters parserName value
              let validationResult = validateElementaryParser parser

              if null parserName && null value
              then validationResult `shouldBe` Left [
                                                ValidationError
                                                  (FieldValidation "name")
                                                  "The name of the parser must not be empty.",
                                                ValidationError
                                                  (FieldValidation "value")
                                                  "A characters parser must contain at least a single char to match."
                                                ]
              else
                if null parserName
                then validationResult `shouldBe` Left[ ValidationError
                                                        (FieldValidation "name")
                                                        "The name of the parser must not be empty."]
                else
                  if null value
                  then validationResult `shouldBe` Left[ ValidationError
                                                          (FieldValidation "value")
                                                          "A characters parser must contain at least a single char to match."]
                  else fromRight' (validateElementaryParser parser) `shouldBe` Just parser



      context "when provided with a valid MatchUntilIncluded ElementaryParser" $ do
        it "returns a <Valid> type containing the parser" $ do
          let parser = MatchUntilIncluded "some parser name" "includedValue"
          fromRight' (validateElementaryParser parser) `shouldBe` Just parser

      context "when provided with an invalid input (empty string)" $ do
        it "returns a correct <ValidationError> list" $ do
            let parser = MatchUntilIncluded "some parser name" ""
            validateElementaryParser parser `shouldBe` Left [ ValidationError
                                                              (FieldValidation "value")
                                                              "A matchUntilIncluded parser must contain at least a single char to match."
                                                            ]

      context "properties" $ do
        prop "returns a <ValidationError> list if invalid and a <Valid> containing the parser otherwise" $
          \parserName value -> do
              let parser = MatchUntilIncluded parserName value
              let validationResult = validateElementaryParser parser

              if null parserName && null value
              then validationResult `shouldBe` Left [
                                                ValidationError
                                                  (FieldValidation "name")
                                                  "The name of the parser must not be empty.",
                                                ValidationError
                                                  (FieldValidation "value")
                                                  "A matchUntilIncluded parser must contain at least a single char to match."
                                                ]
              else
                if null parserName
                then validationResult `shouldBe` Left[ ValidationError
                                                        (FieldValidation "name")
                                                        "The name of the parser must not be empty."]
                else
                  if null value
                  then validationResult `shouldBe` Left[ ValidationError
                                                          (FieldValidation "value")
                                                          "A matchUntilIncluded parser must contain at least a single char to match."]
                  else fromRight' (validateElementaryParser parser) `shouldBe` Just parser



      context "when provided with a valid MatchUntilExcluded ElementaryParser" $ do
        it "returns a <Valid> type containing the parser" $ do
          let parser = MatchUntilExcluded "some parser name" "excludedValue"
          fromRight' (validateElementaryParser parser) `shouldBe` Just parser

      context "when provided with an invalid input (empty string)" $ do
        it "returns a correct <ValidationError> list" $ do
            let parser = MatchUntilExcluded "some parser name" ""
            validateElementaryParser parser `shouldBe` Left [ ValidationError
                                                                (FieldValidation "value")
                                                                "A matchUntilExcluded parser must contain at least a single char to match."
                                                            ]

      context "properties" $ do
        prop "returns a <ValidationError> list if invalid and a <Valid> containing the parser otherwise" $
          \parserName value -> do
              let parser = MatchUntilExcluded parserName value
              let validationResult = validateElementaryParser parser

              if null parserName && null value
              then validationResult `shouldBe` Left [
                                                ValidationError
                                                  (FieldValidation "name")
                                                  "The name of the parser must not be empty.",
                                                ValidationError
                                                  (FieldValidation "value")
                                                  "A matchUntilExcluded parser must contain at least a single char to match."
                                                ]
              else
                if null parserName
                then validationResult `shouldBe` Left[ ValidationError
                                                        (FieldValidation "name")
                                                        "The name of the parser must not be empty."]
                else
                  if null value
                  then validationResult `shouldBe` Left[ ValidationError
                                                          (FieldValidation "value")
                                                          "A matchUntilExcluded parser must contain at least a single char to match."]
                  else fromRight' (validateElementaryParser parser) `shouldBe` Just parser



      context "when provided with a valid MatchFor ElementaryParser" $ do
        it "returns a <Valid> type containing the parser" $ do
          let parser = MatchFor "some parser name" 5
          fromRight' (validateElementaryParser parser) `shouldBe` Just parser

      context "when provided with an invalid input (count <= 0)" $ do
        it "returns a correct <ValidationError> list" $ do
            let parser = MatchFor "some parser name" 0
            validateElementaryParser parser `shouldBe` Left [ ValidationError
                                                              (FieldValidation "count")
                                                              "A matchFor parser must have a count greater 0."
                                                            ]

      context "properties" $ do
        prop "returns a <ValidationError> list if invalid and a <Valid> containing the parser otherwise" $
          \parserName count -> do
              let parser = MatchFor parserName count
              let validationResult = validateElementaryParser parser

              if null parserName && count <= 0
              then validationResult `shouldBe` Left [
                                                ValidationError
                                                  (FieldValidation "name")
                                                  "The name of the parser must not be empty.",
                                                ValidationError
                                                  (FieldValidation "count")
                                                  "A matchFor parser must have a count greater 0."
                                                ]
              else
                if null parserName
                then validationResult `shouldBe` Left [ ValidationError
                                                        (FieldValidation "name")
                                                        "The name of the parser must not be empty."]
                else
                  if count <= 0
                  then validationResult `shouldBe` Left [ ValidationError
                                                          (FieldValidation "count")
                                                          "A matchFor parser must have a count greater 0."]
                  else fromRight' (validateElementaryParser parser) `shouldBe` Just parser



      context "when provided with a valid MatchUntilEnd ElementaryParser" $ do
        it "returns a <Valid> type containing the parser" $ do
          let parser = MatchUntilEnd "any name is valid"
          fromRight' (validateElementaryParser parser) `shouldBe` Just parser




    describe "validateLogfileParser" $ do
      context "when provided with a valid LogfileParser" $ do
          it "returns a <Valid> type containing the parser" $ do
            let parser = LogfileParser "logfileParserName"
                                      [( "colName"
                                       , OneOf "loglevelParser" ["DEBUG", "INFO", "ERROR"]
                                      )]
            fromRight' (validateLogfileParser parser) `shouldBe` Just parser


      context "when provided with an invalid LogfileParser" $ do
          it "returns a correct <ValidationError> list on empty logfileParserName" $ do
            let parser = LogfileParser "logfileParserName" []
            validateLogfileParser parser `shouldBe` Left [ ValidationError
                                                            (FieldValidation "parsers")
                                                            "A logfile parsers must contain at least one elementary parser."
                                                         ]

          it "returns a correct <ValidationError> list on empty elementary parser list" $ do
            let parser = LogfileParser ""
                                      [( "colName"
                                       , OneOf "loglevelParser" ["DEBUG", "INFO", "ERROR"]
                                      )]
            validateLogfileParser parser `shouldBe` Left [
                                                            ValidationError
                                                              (FieldValidation "name")
                                                              "The name of the parser must not be empty."
                                                         ]

          it "returns a correct <ValidationError> list on 2 invalid elementary parsers" $ do
            let parser = LogfileParser ""
                                      [ ( ""
                                        , OneOf "loglevelParser" ["DEBUG", "INFO", "ERROR"]
                                        ),
                                        ( "colName"
                                         , Time "timeParser" ".HHMM"
                                        )
                                      ]
            validateLogfileParser parser `shouldBe` Left [ ValidationError
                                                             (FieldValidation "name")
                                                             "The name of the parser must not be empty."
                                                         , ValidationError
                                                             (FieldValidation "parsers")
                                                             "All parsers in the list must be valid."
                                                         ]




      describe "appendError" $ do
        context "when passing an <ValidationError> and an error list" $ do
          it "returns a list containing the error and the previous list elements" $ do
            let errorToAppend = ValidationError
                                  (FieldValidation "name")
                                  "The name of the parser must not be empty."
            let previousList = [ ValidationError
                                  (FieldValidation "parsers")
                                  "All parsers in the list must be valid."
                               ]
            appendError ( Left errorToAppend ) previousList `shouldBe` errorToAppend : previousList

        context "when passing a <Valid> type and an error list" $ do
          it "returns the unchanged error list" $ do
            let previousList = [ ValidationError
                                (FieldValidation "name")
                                "The name of the parser must not be empty."
                             ]
            appendError (validateParserName "parserName") previousList `shouldBe` previousList





fromRight' :: (Show a, Eq a) => Either b (Valid a) -> Maybe a
fromRight' (Right val) = Just (fromValid val)
fromRight' _ = Nothing
