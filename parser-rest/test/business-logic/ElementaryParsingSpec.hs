module ElementaryParsingSpec
( spec
) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as QC

import CustomParsers
import ElementaryParsing



spec :: Spec
spec = do
    describe "applyParser" $ do
        context "when provided with a valid target value and an elementary parser" $ do
            prop "matches a value if it is in a OneOf parsers list" $ do
                \name -> QC.forAll (QC.listOf1 nonEmptyString) $ \values -> QC.forAll (QC.elements values) $ \val ->
                    applyParser val (OneOf name values) `shouldBe` Right (OneOfResult val)
                -- \name -> QC.forAll (listOf1 values) $ \val ->
                --     applyParser val (OneOf name values) `shouldBe` Right (OneOfResult val)
                -- \name values -> do
                --     let parser = OneOf name values
                --     -- map (\val -> applyParser val parser `shouldBe` (Right (OneOfResult val))) values
                --     applyParser (head values) (OneOf name values) `shouldBe` Right (OneOfResult (head values))
                --     -- (applyParser "asdf" (OneOf "check" ["asdf"])) `shouldBe` Right (OneOfResult "asdf")



    describe "chooseParser" $ do
        context "when provided with a valid input" $ do
            it "TODO" $ do
                pending




nonEmptyString :: QC.Gen String
nonEmptyString = QC.listOf1 QC.arbitrary
