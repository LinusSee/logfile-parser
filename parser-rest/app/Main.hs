module Main where

import Api
import qualified Data.Text.IO as TIO

import qualified Configs as Configs


main :: IO ()
main = do
  configFile <- TIO.readFile "assets/config/project-environment.ini"
  let maybeFileConfig = Configs.readFileDbConfig configFile
  let maybeApiConfig = Configs.readApiConfig configFile

  case maybeFileConfig of
    Right fileConfig -> do
      case maybeApiConfig of
        Right apiConfig -> do
          startApp $ Configs.Config fileConfig apiConfig


        _ -> putStrLn "Failed to read api config"

    _ -> putStrLn "Failed to read fileDb config"
