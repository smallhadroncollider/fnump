module Nump
  ( nump
  , rename
  ) where

import           Control.Monad      (void, when)
import           Data.List          (sortBy)
import           Data.Maybe         (catMaybes, isJust)
import           System.Directory   (getCurrentDirectory, listDirectory,
                                     renameFile)
import           System.Environment (getArgs)
import           Text.Read          (readMaybe)

type Renaming = (FilePath, FilePath)

checkFormat :: String -> Maybe Int
checkFormat str
  | length str /= 2 = Nothing
  | otherwise = readMaybe str :: Maybe Int

zeroPad :: Int -> String
zeroPad i =
  if i > 0 && i < 10
    then "0" ++ show i
    else show i

check :: Int -> Int -> Maybe Int
check start val =
  if val >= start
    then Just val
    else Nothing

biggerThan :: Int -> FilePath -> Maybe Int
biggerThan start path = checkFormat (take 2 path) >>= check start

renaming :: FilePath -> Int -> Renaming
renaming path pre = (path, zeroPad (pre + 1) ++ drop 2 path)

rename :: Int -> FilePath -> Maybe Renaming
rename start path = renaming path <$> biggerThan start path

change :: FilePath -> Renaming -> IO ()
change dir (old, new) = renameFile (ap old) (ap new)
  where
    ap s = dir ++ "/" ++ s

format :: Renaming -> String
format (x, y) = x ++ " -> " ++ y

bump :: Int -> IO ()
bump start = do
  dir <- getCurrentDirectory
  -- reverse order so that renaming doesn't overwrite anything
  files <- sortBy (flip compare) <$> listDirectory dir
  let changes = catMaybes $ rename start <$> files
  putStrLn "Make following changes?"
  putStrLn . unlines $ format <$> changes
  putStrLn "y/N:"
  value <- getLine
  when (value == "y" || value == "Y") $ void . sequence $ change dir <$> changes

-- initial
parseArgs :: [String] -> Maybe Int
parseArgs [value] = checkFormat value
parseArgs _       = Nothing

nump :: IO ()
nump = do
  value <- parseArgs <$> getArgs
  case value of
    Just start -> bump start
    Nothing -> putStrLn "Invalid format: must be two digit number (e.g. 01, 22)"
