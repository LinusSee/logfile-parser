{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Text.Parsec as Parsec
import Data.Time
import Lib

parse rule text = Parsec.parse rule "(source)" text
dateInput :: String
dateInput = "2011-11-23 17:08:00,769 random continuation"

fullInputSample :: String
fullInputSample = "2011-11-23 17:08:00,769 WARN random continuation to have a dummy message \n2011-11-23 17:08:00,769 DEBUG stuff continues"

--printParsedFile :: IO ()
printParsedFile (Left l) = sequence [print l]
printParsedFile (Right r) = sequence $ map print r

main :: IO ()
--main = putStrLn (show $ parse dateTimeParser dateInput)
--main = putStrLn (show $ parse toParseThemAll fullInputSample)
--main = putStrLn (show $ parse fileParser fullInputSample)
main = do
  contents <- readFile "./assets/sample_log4j.log"
  let result = parse fileParser contents
  --putStrLn (show result)
  --sequence (map print result)
  printParsedFile result
  return ()
--main = putStrLn $ show (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S,%q" ("2019-08-31 17:08:00,769" ++ replicate 9 '0') :: Maybe UTCTime)
--main = someFunc
