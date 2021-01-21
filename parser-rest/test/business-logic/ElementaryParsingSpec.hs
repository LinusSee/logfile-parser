module ElementaryParsingSpec
( spec
) where

import Data.List (delete, isPrefixOf)
import Data.Either (isLeft)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as QC

import CustomParsers
import ElementaryParsing



spec :: Spec
spec = do
    -- TODO: Improve readability/descriptions (ugly edge cases)
    describe "applyParser" $ do
        context "when provided with a valid target value and an OneOf parser" $ do
            it "returns the first match even if another would fit" $ do
                let parser = OneOf "name" ["matchNormal", "other", "matchNormalExtended"]
                applyParser "matchNormalExtended" parser `shouldBe` Right (OneOfResult "matchNormal")

            prop "matches a value if it is in a OneOf parsers list (with smaller matches removed)" $ do
                \name -> QC.forAll (QC.listOf1 nonEmptyString) $ \values -> QC.forAll (QC.elements values) $ \val -> do
                    -- Remove elements that start the same as the target element
                    let validValue = \valueToCheck -> not (isPrefixOf valueToCheck val) || valueToCheck == val
                    let validValues = filter validValue values
                    let parser = (OneOf name validValues)

                    applyParser val parser `shouldBe` Right (OneOfResult val)


        context "when provided with an invalid target value and an OneOf parser" $ do
            prop "does not match a value if it isn't in a OneOf parsers list (and no smaller matches exist)" $ do
                \name -> QC.forAll (QC.listOf1 nonEmptyString) $ \values -> QC.forAll (QC.elements values) $ \val -> do
                    -- Remove elements that start the same as the target element
                    let validValue = \valueToCheck -> not (isPrefixOf valueToCheck val)
                    let validValues = filter validValue values
                    let parser = (OneOf name validValues)

                    applyParser val parser `shouldSatisfy` isLeft


        context "when provided with a valid target value and a Time parser" $ do
            it "returns a time by matching the pattern" $ do
              pending


        -- Use the newly learned knowledge about generators to generate patterns


    describe "chooseParser" $ do
        context "when provided with a valid input" $ do
            it "TODO" $ do
                pending




nonEmptyString :: QC.Gen String
nonEmptyString = QC.listOf1 QC.arbitrary


validTimePattern :: QC.Gen String
validTimePattern = do
    first <- QC.elements timePatternBlocks
    let second = head (delete first timePatternBlocks)
    separator <- QC.arbitrary
    return $ first ++ (separator : second)


timePatternBlocks :: [String]
timePatternBlocks = ["HH", "MM"]
