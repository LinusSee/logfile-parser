{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Text.Parsec as Parsec
import Lib

parse rule text = Parsec.parse rule "(source)" text
dateInput :: String
dateInput = "2011-11-23 random continuation"


main :: IO ()
main = putStrLn (show $ parse dateParser dateInput)
--main = someFunc
