{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Nump
  ( nump
  , rename
  ) where

import           ClassyPrelude
import           System.Directory (getCurrentDirectory, listDirectory,
                                   renameFile)

type TFilePath = Text

-- represents filepath before and after
type Renaming = (TFilePath, TFilePath)

-- checks
checkFormat :: Text -> Maybe Int
checkFormat str
  | length str /= 2 = Nothing
  | otherwise = readMay str :: Maybe Int

checkRange :: Int -> Int -> Maybe Int
checkRange start val =
  if val >= start
    then Just val
    else Nothing

include :: Int -> TFilePath -> Maybe Int
include start path = checkFormat (take 2 path) >>= checkRange start

-- renaming
zeroPad :: Int -> Text
zeroPad i =
  if i > 0 && i < 10
    then "0" ++ tshow i
    else tshow i

renaming :: TFilePath -> Int -> Renaming
renaming path pre = (path, zeroPad (pre + 1) ++ drop 2 path)

rename :: Int -> TFilePath -> Maybe Renaming
rename start path = renaming path <$> include start path

change :: TFilePath -> Renaming -> IO ()
change dir (old, new) = renameFile (ap old) (ap new)
  where
    ap s = unpack $ dir ++ "/" ++ s

-- CLI output
format :: Renaming -> Text
format (x, y) = x ++ " -> " ++ y

prompt :: Text -> IO Text
prompt s = do
  putStr $ s ++ ": "
  hFlush stdout -- prevents buffering
  getLine

-- main function
bump :: Int -> IO ()
bump start = do
  dir <- getCurrentDirectory
  -- reverse order so that renaming doesn't overwrite anything
  files <- sortBy (flip compare) <$> listDirectory dir
  -- work out changes
  let changes = catMaybes $ rename start . pack <$> files
  -- confirm changes
  putStrLn . unlines $ format <$> changes
  value <- prompt "Make changes? (y/N)"
  -- if confirmed, make changes
  when (value == "y" || value == "Y") $
    void . sequence $ change (pack dir) <$> changes

-- initial
parseArgs :: [Text] -> Maybe Int
parseArgs [value] = checkFormat value
parseArgs _       = Nothing

nump :: IO ()
nump = do
  value <- parseArgs <$> getArgs
  case value of
    Just start -> bump start
    Nothing -> putStrLn "Invalid format: must be two digit number (e.g. 01, 22)"
