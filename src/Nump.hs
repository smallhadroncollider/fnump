module Nump
  ( nump
  ) where

import           Data.List        (sortBy)
import           Data.Maybe       (isJust)
import           System.Directory
import           Text.Read        (readMaybe)

checkFormat :: String -> Bool
checkFormat str = length str == 2 && isJust (readMaybe str :: Maybe Int)

zeroPad :: Int -> String
zeroPad i =
  if i > 0 && i < 10
    then "0" ++ show i
    else show i

biggerThan :: String -> FilePath -> Bool
biggerThan str fp = pre >= val
  where
    val = read str :: Int
    pre = (read $ take 2 fp) :: Int

filter' :: String -> [FilePath] -> [FilePath]
filter' str fs = filter (biggerThan str) $ filter (checkFormat . take 2) fs

-- bumping
rename :: FilePath -> FilePath -> IO FilePath
rename dir fp = do
  let pre = zeroPad $ (read (take 2 fp) :: Int) + 1
      end = drop 2 fp
      new = pre ++ end
  renameFile (dir ++ "/" ++ fp) (dir ++ "/" ++ new)
  return new

listFiles :: String -> IO [FilePath]
listFiles str = do
  dir <- getCurrentDirectory
  files <- sortBy (flip compare) . filter' str <$> getDirectoryContents dir
  sequence $ rename dir <$> files

bump :: String -> IO ()
bump str = do
  files <- listFiles str
  putStrLn $ unlines files

-- initial
nump :: IO ()
nump = do
  putStrLn "Start value:"
  value <- getLine
  if checkFormat value
    then bump value
    else putStrLn "Invalid format: must be two digit number (e.g. 01, 22)"
