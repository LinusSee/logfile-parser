module ElementaryParsingSpec
( spec
) where

import Data.List (delete, isPrefixOf, isSuffixOf)
import Data.Either (isLeft)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as QC
import Data.Time (TimeOfDay (..), Day)

import CustomParsers
import ElementaryParsing



spec :: Spec
spec = do
    -- TODO: Improve readability/descriptions (ugly edge cases)
    describe "applyParser - OneOf Parser" $ do
        context "when provided with a valid target value and an OneOf parser" $ do
            it "returns the first match even if another would fit" $ do
                let parser = OneOf ["matchNormal", "other", "matchNormalExtended"]
                applyParser "matchNormalExtended" parser `shouldBe` Right (OneOfResult "matchNormal")

            prop "matches a value if it is in a OneOf parsers list (with smaller matches removed)" $ do
                QC.forAll (QC.listOf1 nonEmptyString) $ \values -> QC.forAll (QC.elements values) $ \val -> do
                    -- Remove elements that start the same as the target element
                    let validValue = \valueToCheck -> not (isPrefixOf valueToCheck val) || valueToCheck == val
                    let validValues = filter validValue values
                    let parser = OneOf validValues

                    applyParser val parser `shouldBe` Right (OneOfResult val)


        context "when provided with an invalid target value and an OneOf parser" $ do
            prop "does not match a value if it isn't in a OneOf parsers list (and no smaller matches exist)" $ do
                QC.forAll (QC.listOf1 nonEmptyString) $ \values -> QC.forAll (QC.elements values) $ \val -> do
                    -- Remove elements that start the same as the target element
                    let validValue = \valueToCheck -> not (isPrefixOf valueToCheck val)
                    let validValues = filter validValue values
                    let parser = OneOf validValues

                    applyParser val parser `shouldSatisfy` isLeft



    describe "applyParser - Time Parser" $ do
        context "when provided with a valid target value and a Time parser" $ do
            it "returns a time by matching the pattern HH:MM" $ do
                let target = "11:59 and some more text"
                let parser = Time "HH:MM"

                applyParser target parser `shouldBe` Right (TimeResult $ TimeOfDay 11 59 00 )


            it "returns a time by matching the pattern HH_MM" $ do
                let target = "14_01"
                let parser = Time "HH_MM"

                applyParser target parser `shouldBe` Right (TimeResult $ TimeOfDay 14 01 00)


            it "returns a time by matching the pattern MM-HH" $ do
                let target = "00-0 and some more text"
                let parser = Time "MM-HH"

                applyParser target parser `shouldBe` Right (TimeResult $ TimeOfDay 0 0 0)


        context "when provided with an invalid target value and a Time parser" $ do
            it "returns an error if the separator doesn't match the pattern" $ do
                let target = "10:00 and some more text"
                let parser = Time "HH-MM"

                applyParser target parser `shouldSatisfy` isLeft


            it "returns an error if the target doesn't match the pattern" $ do
                let target = "1-a00 and some more text"
                let parser = Time "HH-MM"

                applyParser target parser `shouldSatisfy` isLeft


            it "returns an error if the hour is not a valid one" $ do
                let target = "24-00 and some more text"
                let parser = Time "HH-MM"

                applyParser target parser `shouldSatisfy` isLeft


            it "returns an error if the minute is not a valid one" $ do
                let target = "10-60 and some more text"
                let parser = Time "HH-MM"

                applyParser target parser `shouldSatisfy` isLeft



    -- Use the newly learned knowledge about generators to generate patterns
    describe "applyParser - Date" $ do
        context "when provided with a valid input and a Date parser" $ do
            prop "matches a value that fits a Date parser's pattern" $ do
                QC.forAll validDatePattern $
                  \pattern -> QC.forAll (simpleDateFromPattern pattern) $ \date -> do
                    let parser = Date pattern
                    let result = Right ( DateResult $ toDate pattern date )

                    applyParser date parser `shouldBe` result



    describe "applyParser - Characters" $ do
        context "when provided with a valid input and a Characters parser" $ do
            prop "matches the provided string" $ do
                \target -> do
                    let parser = Characters target

                    applyParser target parser `shouldBe` Right (CharactersResult target)

        context "when provided with an invalid input and a Characters parser" $ do
            prop "doesn't match the provided string" $ do
                QC.forAll (QC.listOf1 QC.arbitrary) $ \target -> do
                    let parser = Characters target

                    applyParser (drop 1 target) parser `shouldSatisfy` isLeft




    describe "applyParser - MatchUntilIncluded" $ do
        context "when provided with a valid input and a MatchUntilIncluded parser" $ do
            prop "matches until the provided string, with the string being included" $ do
                \before after -> QC.forAll (QC.listOf1 QC.arbitrary) $ \target -> do
                    let parser = MatchUntilIncluded target
                    let stringToParse = takeUntilSubstring before target ++ target ++ after
                    let result = takeUntilSubstring before target ++ target

                    applyParser stringToParse parser `shouldBe` Right (MatchUntilIncludedResult result)


        context "when provided with an invalid input and a MatchUntilIncluded parser" $ do
            prop "doesn't match the provided string" $ do
                \before -> QC.forAll (QC.listOf1 QC.arbitrary) $ \target -> do
                    let parser = MatchUntilIncluded target
                    let stringToParse = takeUntilSubstring before target

                    applyParser stringToParse parser `shouldSatisfy` isLeft




    describe "applyParser - MatchUntilExcluded" $ do
        context "when provided with a valid input and a MatchUntilExcluded parser" $ do
            prop "matches until the provided string, with the provided string being excluded" $ do
                \before after -> QC.forAll (QC.listOf1 QC.arbitrary) $ \target -> do
                    let parser = MatchUntilExcluded target
                    let stringToParse = takeUntilSubstring before target ++ target ++ after
                    let result = takeUntilSubstring before target

                    applyParser stringToParse parser `shouldBe` Right (MatchUntilExcludedResult result)


        context "when provided with an invalid input and a MatchUntilExcluded parser" $ do
            prop "doesn't match the provided string" $ do
                \before -> QC.forAll (QC.listOf1 QC.arbitrary) $ \target -> do
                    let parser = MatchUntilExcluded target
                    let stringToParse = takeUntilSubstring before target

                    applyParser stringToParse parser `shouldSatisfy` isLeft



    describe "applyParser - MatchFor" $ do
        context "when provided with a valid input and a MatchFor parser" $ do
            prop "matches the first n characters" $ do
                \count -> QC.forAll (certainLengthString count) $ \target -> do
                    let parser = MatchFor count

                    applyParser target parser `shouldBe` Right (MatchForResult (take count target))



    describe "applyParser - MatchUntilEnd" $ do
        context "when provided with a valid input without newline and a MatchUntil parser" $ do
            prop "matches the entire string" $ do
                \start end -> do
                    let parser = MatchUntilEnd
                    let target = filter ((/=)'\r') $ filter ((/=)'\n') (start ++ end)

                    applyParser target parser `shouldBe` Right (MatchUntilEndResult target)


            prop "matches until newline (\\n)" $ do
                \start -> QC.forAll (QC.listOf1 QC.arbitrary) $ \end -> do
                    let parser = MatchUntilEnd
                    let cleanStart = filter ((/=) '\r') $ filter ((/=) '\n') start
                    let target = cleanStart ++ "\n" ++ end

                    applyParser target parser `shouldBe` Right (MatchUntilEndResult cleanStart)


            prop "matches until newline (\\r\\n)" $ do
                \start -> QC.forAll (QC.listOf1 QC.arbitrary) $ \end -> do
                    let parser = MatchUntilEnd
                    let cleanStart = filter ((/=) '\r') $ filter ((/=) '\n') start
                    let target = cleanStart ++ "\r\n" ++ end

                    applyParser target parser `shouldBe` Right (MatchUntilEndResult cleanStart)



takeUntilSubstring :: String -> String -> String
takeUntilSubstring [] _ = []
takeUntilSubstring l [] = l
takeUntilSubstring (x:xs) (y:ys)
  | x == y && ys `isPrefixOf` xs = []
  | otherwise = x : takeUntilSubstring xs (y:ys)




certainLengthString :: Int -> QC.Gen String
certainLengthString 1 = do
    char <- QC.arbitrary
    return $ char : ""
certainLengthString n = do
    char <- QC.arbitrary
    end <- certainLengthString (n - 1)
    return $ char : end


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
-- QC.listOf $ QC.elements "HM"


validDatePattern :: QC.Gen String
validDatePattern = do
    first <- QC.elements datePatternBlocks
    second <- QC.elements $ delete first datePatternBlocks
    third <- QC.elements $ delete first $ delete second datePatternBlocks

    sep1 <- QC.arbitrary
    sep2 <- QC.arbitrary

    return $ first ++ (sep1 : second) ++ (sep2 : third)


-- Ignore edge cases like leap year and what not
simpleDateFromPattern :: String -> QC.Gen String
simpleDateFromPattern pattern = do
    year <- QC.elements [1970..2200]
    month <- QC.elements $ map ((++) "0" . show) [1..9] ++ map show [10..12]
    day <- dayForMonth month

    return $ replaceDatePattern "YYYY" (show year)
           $ replaceDatePattern "MM" month
           $ replaceDatePattern "DD" day pattern


dayForMonth :: String -> QC.Gen String
dayForMonth month
    | month == "02"                           = QC.elements (untilNine ++ map show [10..28])
    | month `elem` ["01", "03", "05", "07", "08", "10", "12"] = QC.elements (untilNine ++ map show [10..31])
    | otherwise                            = QC.elements (untilNine ++ map show [10..30])

    where untilNine = ["01", "02", "03", "04", "05", "06", "07", "08", "09"]


replacePattern :: String -> String -> String -> String
replacePattern _ _ [] = []
replacePattern pattern newVal target
    | pattern `isPrefixOf` target = newVal ++ drop (length pattern) target
    | otherwise                   = head target : replacePattern pattern newVal (tail target)


replaceDatePattern :: String -> String -> String -> String
replaceDatePattern _ _ [] = []
replaceDatePattern pattern newVal target
    | pattern `isPrefixOf` target  = newVal ++ drop (length pattern) target
    | pattern `isSuffixOf` target = take (length target - length pattern) target ++ newVal
    | "YYYY" `isPrefixOf` target   = take 5 target ++ newVal ++ drop 7 target
    | otherwise                    = take 3 target ++ newVal ++ drop (length pattern + 3) target


extractAtPattern :: String -> String -> String -> String
extractAtPattern _ [] _ = []
extractAtPattern toExtract pattern target
    | toExtract `isPrefixOf` pattern = take (length toExtract) target
    | otherwise                      = extractAtPattern toExtract (tail pattern) (tail target)


toDate :: String -> String -> Day
toDate pattern date = read $
  ( extractDatePattern "YYYY" pattern date ++ "-"
  ++ extractDatePattern "MM" pattern date   ++ "-"
  ++ extractDatePattern "DD" pattern date
  )

extractDatePattern :: String -> String -> String -> String
extractDatePattern _ _ [] = []
extractDatePattern toExtract pattern target
    | toExtract `isPrefixOf` pattern  = take (length toExtract) target
    | toExtract `isSuffixOf` pattern = drop (length target - length toExtract) target
    | "YYYY" `isPrefixOf` pattern   = take 2 $ drop 5 target
    | otherwise                    = take (length toExtract) $ drop 3 target


dateToTimeFormat :: String -> String -> String
dateToTimeFormat date pattern = "Hey"


datePatternBlocks :: [String]
datePatternBlocks = ["YYYY", "MM", "DD"]
