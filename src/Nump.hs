module Nump
  ( nump
  , filter'
  ) where

import           Data.List        (sortBy)
import           Data.Maybe       (isJust)
import           System.Directory
import           Text.Read        (readMaybe)

checkFormat :: String -> Maybe Int
checkFormat str
  | length str /= 2 = Nothing
  | otherwise = readMaybe str :: Maybe Int

zeroPad :: Int -> String
zeroPad i =
  if i > 0 && i < 10
    then "0" ++ show i
    else show i

biggerThan :: Int -> FilePath -> Bool
biggerThan start fp =
  case checkFormat $ take 2 fp of
    Nothing  -> False
    Just val -> val >= start

filter' :: Int -> [FilePath] -> [FilePath]
filter' val = filter (biggerThan val)

-- bumping
rename :: FilePath -> FilePath -> IO FilePath
rename dir fp = do
  let pre = zeroPad $ (read (take 2 fp) :: Int) + 1
      end = drop 2 fp
      new = pre ++ end
  renameFile (dir ++ "/" ++ fp) (dir ++ "/" ++ new)
  return new

listFiles :: Int -> IO [FilePath]
listFiles start = do
  dir <- getCurrentDirectory
  files <- sortBy (flip compare) . filter' start <$> getDirectoryContents dir
  sequence $ rename dir <$> files

bump :: Int -> IO ()
bump start = do
  files <- listFiles start
  putStrLn $ unlines files

-- initial
nump :: IO ()
nump = do
  putStrLn "Start value:"
  value <- getLine
  case checkFormat value of
    Nothing -> putStrLn "Invalid format: must be two digit number (e.g. 01, 22)"
    Just start -> bump start
