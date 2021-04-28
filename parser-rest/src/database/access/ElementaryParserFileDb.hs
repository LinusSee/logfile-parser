module ElementaryParserFileDb
  ( readAllIds
  , readAll
  , save
  ) where

import qualified Data.Text as T
import Control.Monad (when)
import Data.UUID
import Data.UUID.V4 (nextRandom)

import qualified Configs as Configs

import qualified DbParserModels as DM



readAllIds :: Configs.FileDbConfig -> IO [DM.ElementaryParserId]
readAllIds dbConfig = do
  entities <- readAllEntities dbConfig
  return $ map extractIdAndName entities

  where extractIdAndName = \(DM.Entity uuid (DM.ElementaryParser name _ _)) -> DM.ElementaryParserId uuid name


readAll :: Configs.FileDbConfig -> IO ([DM.ElementaryParser])
readAll dbConfig = do
  entities <- readAllEntities dbConfig
  return $ map extractEntity entities

  where extractEntity = \(DM.Entity _ entity) -> entity


readAllEntities :: Configs.FileDbConfig -> IO ([DM.Entity DM.ElementaryParser])
readAllEntities dbConfig = do
  contents <- readFile filePath

  return (toParsers contents)

  where filePath = T.unpack $ Configs.elementaryParserPath dbConfig


save :: Configs.FileDbConfig -> DM.ElementaryParser -> IO UUID
save dbConfig parser = do
  newUUID <- nextRandom
  let newElementaryParser = DM.Entity newUUID parser
  allParsers <- readAllEntities dbConfig

  -- Needed because of lazy IO
  when (length allParsers >= 0) $
    writeFile filePath $ show (newElementaryParser:allParsers)

  return newUUID

  where filePath = T.unpack $ Configs.elementaryParserPath dbConfig


toParsers :: String -> [DM.Entity DM.ElementaryParser]
toParsers contents = read contents
