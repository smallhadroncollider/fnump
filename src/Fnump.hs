{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fnump
    ( fnump
    , rename
    ) where

import ClassyPrelude
import Data.Text        (replace)
import Data.Text.IO     (hPutStrLn)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory, renameFile)
import System.Exit      (exitFailure)

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

-- updating
rep :: Text -> Renaming -> Text
rep content (old, new) = replace old new content

update :: ReplaceIn -> [Renaming] -> IO ()
update (Requested file) changes = do
    let path = unpack file
    content <- decodeUtf8 <$> readFile path
    writeFile path $ encodeUtf8 (foldl' rep content changes)
update _ _ = return ()

-- CLI output
format :: Renaming -> Text
format (x, y) = x ++ " -> " ++ y

prompt :: Text -> IO Text
prompt s = do
    putStr $ s ++ ": "
    hFlush stdout -- prevents buffering
    getLine

exitWithErrorMessage :: Text -> IO ()
exitWithErrorMessage str = hPutStrLn stderr str >> void exitFailure

-- main function
bump :: Int -> ReplaceIn -> IO ()
bump start file = do
    dir <- getCurrentDirectory
    -- reverse order so that renaming doesn't overwrite anything
    files <- sortBy (flip compare) <$> listDirectory dir
    -- work out changes
    let changes = catMaybes $ rename start . pack <$> files
    if null changes
        then putStrLn "No changes necessary"
        -- confirm changes
        else do
            putStrLn . unlines $ format <$> changes
            value <- prompt "Make changes? (y/N)"
            -- if confirmed, make changes
            if value == "y" || value == "Y"
                then do
                    void . sequence $ change (pack dir) <$> changes -- rename files
                    void $ update file changes -- update file (if given)
                else putStrLn "Operation cancelled"

-- initial
parseArgs :: [Text] -> (Maybe Int, Maybe TFilePath)
parseArgs [value]       = (checkFormat value, Nothing)
parseArgs [value, file] = (checkFormat value, Just file)
parseArgs _             = (Nothing, Nothing)

data ReplaceIn
    = Requested TFilePath
    | NotFound TFilePath
    | NotRequested

replaceIn :: Maybe TFilePath -> IO ReplaceIn
replaceIn (Just file) = do
    exists <- doesFileExist (unpack file)
    return $
        if exists
            then Requested file
            else NotFound file
replaceIn Nothing = return NotRequested

fnump :: IO ()
fnump = do
    (value, file) <- parseArgs <$> getArgs
    replaceInFile <- replaceIn file
    case replaceInFile of
        NotFound fp -> exitWithErrorMessage ("File " ++ fp ++ " does not exist")
        _ ->
            case value of
                Just start -> bump start replaceInFile
                Nothing ->
                    exitWithErrorMessage "Invalid format: must be two digit number (e.g. 01, 22)"
