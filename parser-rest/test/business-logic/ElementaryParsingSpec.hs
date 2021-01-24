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
    describe "applyParser - OneOf Parser" $ do
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



    describe "applyParser - Time Parser" $ do
        context "when provided with a valid target value and a Time parser" $ do
            it "returns a time by matching the pattern HH:MM" $ do
                let target = "11:59 and some more text"
                let parser = Time "someName" "HH:MM"

                applyParser target parser `shouldBe` Right (TimeResult "11:59:00")


            it "returns a time by matching the pattern HH_MM" $ do
                let target = "14_01"
                let parser = Time "someName" "HH_MM"

                applyParser target parser `shouldBe` Right (TimeResult "14:01:00")


            it "returns a time by matching the pattern MM-HH" $ do
                let target = "00-0 and some more text"
                let parser = Time "someName" "MM-HH"

                applyParser target parser `shouldBe` Right (TimeResult "00:00:00")


        context "when provided with an invalid target value and a Time parser" $ do
            it "returns an error if the separator doesn't match the pattern" $ do
                let target = "10:00 and some more text"
                let parser = Time "someName" "HH-MM"

                applyParser target parser `shouldSatisfy` isLeft


            it "returns an error if the target doesn't match the pattern" $ do
                let target = "1-a00 and some more text"
                let parser = Time "someName" "HH-MM"

                applyParser target parser `shouldSatisfy` isLeft


            it "returns an error if the hour is not a valid one" $ do
                let target = "24-00 and some more text"
                let parser = Time "someName" "HH-MM"

                applyParser target parser `shouldSatisfy` isLeft


            it "returns an error if the minute is not a valid one" $ do
                let target = "10-60 and some more text"
                let parser = Time "someName" "HH-MM"

                applyParser target parser `shouldSatisfy` isLeft


        -- Use the newly learned knowledge about generators to generate patterns


    describe "applyParser" $ do
        context "when provided with a valid input" $ do
            it "matches a value that fits a Time parser's pattern" $ do
                pending



    describe "applyParser - Characters" $ do
        context "when provided with a valid input and a Characters parser" $ do
            prop "matches the provided string" $ do
                \name target -> do
                    let parser = Characters name target

                    applyParser target parser `shouldBe` Right (CharactersResult target)

        context "when provided with an invalid input and a Characters parser" $ do
            prop "doesn't match the provided string" $ do
                \name -> QC.forAll (QC.listOf1 QC.arbitrary) $ \target -> do
                    let parser = Characters name target

                    applyParser (drop 1 target) parser `shouldSatisfy` isLeft




    describe "applyParser - MatchUntilIncluded" $ do
        context "when provided with a valid input and a MatchUntilIncluded parser" $ do
            prop "matches until the provided string, with the string being included" $ do
                \name before after -> QC.forAll (QC.listOf1 QC.arbitrary) $ \target -> do
                    let parser = MatchUntilIncluded name target
                    let stringToParse = takeUntilSubstring before target ++ target ++ after
                    let result = takeUntilSubstring before target ++ target

                    applyParser stringToParse parser `shouldBe` Right (MatchUntilIncludedResult result)


        context "when provided with an invalid input and a MatchUntilIncluded parser" $ do
            prop "doesn't match the provided string" $ do
                \name before -> QC.forAll (QC.listOf1 QC.arbitrary) $ \target -> do
                    -- Testdata could be incorrect in some rare constellations (check if problems occur)
                    let parser = MatchUntilIncluded name target
                    let stringToParse = takeUntilSubstring before target ++ drop 1 target

                    applyParser stringToParse parser `shouldSatisfy` isLeft




    describe "applyParser - MatchUntilExcluded" $ do
        context "when provided with a valid input and a MatchUntilExcluded parser" $ do
            prop "matches until the provided string, with the provided string being excluded" $ do
                \name before after -> QC.forAll (QC.listOf1 QC.arbitrary) $ \target -> do
                    let parser = MatchUntilExcluded name target
                    let stringToParse = takeUntilSubstring before target ++ target ++ after
                    let result = takeUntilSubstring before target

                    applyParser stringToParse parser `shouldBe` Right (MatchUntilExcludedResult result)


        context "when provided with an invalid input and a MatchUntilExcluded parser" $ do
            prop "doesn't match the provided string" $ do
                \name before -> QC.forAll (QC.listOf1 QC.arbitrary) $ \target -> do
                    -- Testdata could be incorrect in some rare constellations (check if problems occur)
                    let parser = MatchUntilExcluded name target
                    let stringToParse = takeUntilSubstring before target ++ drop 1 target

                    applyParser stringToParse parser `shouldSatisfy` isLeft



    describe "applyParser" $ do
        context "when provided with a valid input" $ do
            it "matches a value that fits a Time parser's pattern" $ do
                pending



    describe "applyParser" $ do
        context "when provided with a valid input" $ do
            it "matches a value that fits a Time parser's pattern" $ do
                pending



takeUntilSubstring :: String -> String -> String
takeUntilSubstring [] _ = []
takeUntilSubstring l [] = l
takeUntilSubstring (x:xs) (y:ys)
  | x == y && ys `isPrefixOf` xs = []
  | otherwise = x : takeUntilSubstring xs (y:ys)




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
