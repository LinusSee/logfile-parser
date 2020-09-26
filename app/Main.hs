{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Text.Parsec as Parsec
import Data.Time

import Lib (fileParser)

parse rule text = Parsec.parse rule "Logfile parser (source name)" text

printParsedFile :: (Show a, Show b) => Either a [b] -> IO [()]
printParsedFile (Left l) = sequence [print l]
printParsedFile (Right r) = sequence $ map print r

main :: IO [()]
main = do
  logContents <- readFile "./assets/sample_log4j.log"
  let result = parse fileParser logContents
  printParsedFile result
