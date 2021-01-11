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
          it "returns a <Right> type" $ do
              validateTarget "a valid string" `shouldSatisfy` Either.isRight


          it "returns a <Valid> type containing the target string if it is not null" $ do
              fromRight' (validateTarget "a valid string") `shouldBe` Just "a valid string"



      context "when provided with an invalid input (empty string)" $ do
          it "returns a <Left> type if the target string is empty" $ do
              validateTarget "" `shouldSatisfy` Either.isLeft


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




fromRight' :: (Show a, Eq a) => Either ValidationError (Valid a) -> Maybe a
fromRight' (Right val) = Just (fromValid val)
fromRight' _ = Nothing
