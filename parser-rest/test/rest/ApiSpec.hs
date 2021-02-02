module ApiSpec
( spec
) where


import Test.Hspec

import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)


createDbFiles :: IO ()
createDbFiles = do
    createDirectoryIfMissing True "assets/test_db"
    writeFile "assets/test_db/parsers.txt" $ show ([] :: [Int])


clearDbFiles :: IO ()
clearDbFiles = removeDirectoryRecursive "assets/test_db"


spec :: Spec
spec = before_ createDbFiles $
       after_ clearDbFiles $ do
         describe "api" $ do
           it "returns" $ do
             pending
