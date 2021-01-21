module LogfileParsingSpec
( spec
) where

import Test.Hspec

spec :: Spec
spec =
    describe "applyLogfileParser" $ do
        context "when provided with a valid input" $ do
            it "returns" $ do
                pending
