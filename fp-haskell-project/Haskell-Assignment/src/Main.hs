module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import Data.List qualified as L
import Entry.DB qualified as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import GHC.Exts.Heap (StgInfoTable (entry))
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import Prelude qualified

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: (TestableMonadIO m) => m ()
handleInit = do
  DB.save DB.empty
  return ()

-- | Handle the get command
handleGet :: (TestableMonadIO m) => GetOptions -> m ()
handleGet getOpts = do
  db <- DB.load
  case db of
    Error err -> putStrLn "Failed to load DB"
    Success dbGood -> do
      let id = getOptId getOpts
      case DB.findFirst (\e -> entryId e == id) dbGood of
        Just entry | let snippet = entrySnippet entry -> putStrLn snippet
        _ -> putStrLn "No entry found"
  return ()

-- | Handle the search command
handleSearch :: (TestableMonadIO m) => SearchOptions -> m ()
handleSearch searchOpts = do
  db <- DB.load
  case db of
    Error err -> putStrLn "Failed to load DB"
    Success dbGood -> do
      let entries = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) dbGood
      case entries of
        [] -> putStrLn "No entries found"
        _ -> do
          let fmtEntries = map FmtEntry entries
          mapM_ (putStrLn . show) fmtEntries
  return ()

-- | Handle the add command
handleAdd :: (TestableMonadIO m) => AddOptions -> m ()
handleAdd addOpts = do
  snip <- readFile (addOptFilename addOpts)
  db <- DB.load
  case db of
    Error err -> putStrLn "Failed to load DB"
    Success dbGood -> do
      let -- check if the snippet is already in the db
          result = DB.findFirst (\e -> entrySnippet e == snip) dbGood
      case result of
        Just entry -> do
          let msg = "Entry with this content already exists: "
              entryDetail = head $ lines $ show $ FmtEntry entry
          putStrLn msg
          putStrLn entryDetail
          return ()
        Nothing -> do
          let db' = DB.insertWith (\id -> makeEntry id snip addOpts) dbGood
          DB.save db'
          return ()
  return ()
  where
    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: (TestableMonadIO m) => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
