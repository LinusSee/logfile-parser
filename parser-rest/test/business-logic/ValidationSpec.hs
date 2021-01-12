module ValidationSpec
( spec
) where

import qualified Data.Either as Either
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Validation
import ValidationModels


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
            it "returns a <Valid> type containing the parserName string if it is not null" $ do
                fromRight' (validateParserName "a valid name") `shouldBe` Just "a valid name"



        context "when provided with an invalid input (empty string)" $ do
            it "returns a correct <ValidationError> if parserName string is empty" $ do
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




fromRight' :: (Show a, Eq a) => Either ValidationError (Valid a) -> Maybe a
fromRight' (Right val) = Just (fromValid val)
fromRight' _ = Nothing
