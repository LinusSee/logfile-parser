module ValidationSpec
( spec
) where

import qualified Data.Either as Either
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Validation
import ValidationModels
import CustomParsers


spec :: Spec
spec =
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


        context "when provided with a valid Date ElementaryParser" $ do
          it "returns a <Valid> type containing the parser" $ do
            let parser = Date "some parser name" "YYYY-DD-MM"
            fromRight' (validateElementaryParser parser) `shouldBe` Just parser



        context "when provided with a valid Characters ElementaryParser" $ do
          it "returns a <Valid> type containing the parser" $ do
            let parser = Characters "some parser name" "stringToMatch"
            fromRight' (validateElementaryParser parser) `shouldBe` Just parser



        context "when provided with a valid MatchUntilIncluded ElementaryParser" $ do
          it "returns a <Valid> type containing the parser" $ do
            let parser = MatchUntilIncluded "some parser name" "includedValue"
            fromRight' (validateElementaryParser parser) `shouldBe` Just parser



        context "when provided with a valid MatchUntilExcluded ElementaryParser" $ do
          it "returns a <Valid> type containing the parser" $ do
            let parser = MatchUntilExcluded "some parser name" "excludedValue"
            fromRight' (validateElementaryParser parser) `shouldBe` Just parser



        context "when provided with a valid MatchFor ElementaryParser" $ do
          it "returns a <Valid> type containing the parser" $ do
            let parser = MatchFor "some parser name" 5
            fromRight' (validateElementaryParser parser) `shouldBe` Just parser



        context "when provided with a valid MatchUntilEnd ElementaryParser" $ do
          it "returns a <Valid> type containing the parser" $ do
            let parser = MatchUntilEnd "any name is valid"
            fromRight' (validateElementaryParser parser) `shouldBe` Just parser





fromRight' :: (Show a, Eq a) => Either b (Valid a) -> Maybe a
fromRight' (Right val) = Just (fromValid val)
fromRight' _ = Nothing
