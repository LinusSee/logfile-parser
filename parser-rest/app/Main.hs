module Main where

import Api
import qualified Data.Text.IO as TIO

import qualified FileDbConfig as DbConfig


main :: IO ()
main = do
  configFile <- TIO.readFile "project-environment.ini"

  let maybeConfig = DbConfig.fileDbConfig configFile
    in case maybeConfig of
        Right config -> do
          startApp config
        _ -> putStrLn "Failed to read config file 'project-environtment.ini'"
