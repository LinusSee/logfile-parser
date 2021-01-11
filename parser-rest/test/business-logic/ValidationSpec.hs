module ValidationSpec
( spec
) where

import qualified Data.Either as Either
-- import qualified Data.Either.Combinators as EitherComb
import Test.Hspec

import Validation
import ValidationModels


spec :: Spec
spec =
    describe "validateTarget" $ do
      context "when provided with a valid input (nonempty string)" $ do
          it "returns a <Right> type" $ do
              validateTarget "a valid string" `shouldSatisfy` Either.isRight

          it "returns a <Valid> type containing the target string if it is not null" $ do
              let result = validateTarget "a valid string"

              -- TODO: Maybe refactor somehow? e.g. fail in case statement

              Either.isRight result `shouldBe` True
              map fromValid (Either.rights [result]) `shouldBe` ["a valid string"]

      context "when provided with an invalid input (empty string)" $ do
          it "returns a <Left> type if the target string is empty" $ do
              validateTarget "" `shouldSatisfy` Either.isLeft

          it "returns a correct <ValidationError> if target string is empty" $ do
              validateTarget "" `shouldBe` (Left $ ValidationError
                                              (FieldValidation "target")
                                              "The target string to parse must not be empty.")
