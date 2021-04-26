module LogfileParserFileDb
( readAll
, save
) where

import qualified Data.Text as T
import Control.Monad (when)
import Data.UUID.V4 (nextRandom)

import qualified Configs as Configs

import qualified DbParserModels as DM



readAll :: Configs.FileDbConfig -> IO [DM.LogfileParser]
readAll dbConfig = do
  entities <- readAllEntities dbConfig
  return $ map extractEntity entities

  where extractEntity = \(DM.Entity _ entity) -> entity


readAllEntities :: Configs.FileDbConfig -> IO [DM.Entity DM.LogfileParser]
readAllEntities dbConfig = do
  contents <- readFile filePath

  return (toParsers contents)

  where filePath = T.unpack $ Configs.logfileParserPath dbConfig


save :: Configs.FileDbConfig -> DM.LogfileParser -> IO ()
save dbConfig logfileParser = do
  newUUID <- nextRandom
  let newLogfileParser = DM.Entity newUUID logfileParser
  allParsers <- readAllEntities dbConfig

  -- Needed because of lazy IO
  when (length allParsers >= 0) $
    writeFile filePath $ show (newLogfileParser:allParsers)

  where filePath = T.unpack $ Configs.logfileParserPath dbConfig


toParsers :: String -> [DM.Entity DM.LogfileParser]
toParsers contents = read contents
