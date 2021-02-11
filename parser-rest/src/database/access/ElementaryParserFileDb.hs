module ElementaryParserFileDb
  ( readAll
  , save
  ) where

import qualified Data.Text as T
import Control.Monad (when)

import CustomParsers ( ElementaryParser )
import qualified Configs as Configs



readAll :: Configs.FileDbConfig -> IO ([ElementaryParser])
readAll dbConfig = do
  contents <- readFile filePath

  return (toParsers contents)

  where filePath = T.unpack $ Configs.elementaryParserPath dbConfig


save :: Configs.FileDbConfig -> ElementaryParser -> IO ()
save dbConfig parser = do
  allParsers <- readAll dbConfig

  -- Needed because of lazy IO
  when (length allParsers >= 0) $
    writeFile filePath $ show (parser:allParsers)

  where filePath = T.unpack $ Configs.elementaryParserPath dbConfig


toParsers :: String -> [ElementaryParser]
toParsers contents = read contents
