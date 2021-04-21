module LogfileParserFileDb
( readAll
, save
) where

import qualified Data.Text as T
import Control.Monad (when)

import qualified Configs as Configs

import qualified DbParserModels as DM



readAll :: Configs.FileDbConfig -> IO [DM.LogfileParser]
readAll dbConfig = do
  contents <- readFile filePath

  return (toParsers contents)

  where filePath = T.unpack $ Configs.logfileParserPath dbConfig


save :: Configs.FileDbConfig -> DM.LogfileParser -> IO ()
save dbConfig logfileParser = do
  allParsers <- readAll dbConfig

  -- Needed because of lazy IO
  when (length allParsers >= 0) $
    writeFile filePath $ show (logfileParser:allParsers)

  where filePath = T.unpack $ Configs.logfileParserPath dbConfig


toParsers :: String -> [DM.LogfileParser]
toParsers contents = read contents
