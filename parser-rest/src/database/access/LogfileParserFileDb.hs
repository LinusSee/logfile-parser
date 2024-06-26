module LogfileParserFileDb
( readById
, readAllIds
, readAll
, save
) where

import Data.List as List
import qualified Data.Text as T
import Control.Monad (when)
import Data.UUID
import Data.UUID.V4 (nextRandom)

import qualified Configs as Configs

import qualified DbParserModels as DM



readById :: Configs.FileDbConfig -> UUID -> IO (Maybe DM.LogfileParser)
readById dbConfig uuid = do
  entities <- readAllEntities dbConfig
  let maybeEntity = List.find (matchesId uuid) entities

  return $ fmap extractParser maybeEntity

  where matchesId targetUuid (DM.Entity currentUuid _) = targetUuid == currentUuid
        extractParser (DM.Entity _ parser) = parser


readAllIds :: Configs.FileDbConfig -> IO [DM.LogfileParserId]
readAllIds dbConfig = do
  entities <- readAllEntities dbConfig
  return $ map extractId entities

  where extractId = \(DM.Entity uuid (DM.LogfileParser name _)) -> DM.LogfileParserId uuid name


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


save :: Configs.FileDbConfig -> DM.LogfileParser -> IO UUID
save dbConfig logfileParser = do
  newUUID <- nextRandom
  let newLogfileParser = DM.Entity newUUID logfileParser
  allParsers <- readAllEntities dbConfig

  -- Needed because of lazy IO
  when (length allParsers >= 0) $
    writeFile filePath $ show (newLogfileParser:allParsers)

  return newUUID

  where filePath = T.unpack $ Configs.logfileParserPath dbConfig


toParsers :: String -> [DM.Entity DM.LogfileParser]
toParsers contents = read contents
