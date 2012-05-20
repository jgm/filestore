{-# LANGUAGE CPP #-}
{- |
   Module      : Data.FileStore.Darcs
   Copyright   : Copyright (C) 2009 Gwern Branwen
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : GHC 6.10 required

   A versioned filestore implemented using darcs.
   Normally this module should not be imported: import
   "Data.FileStore" instead. -}

module Data.FileStore.Darcs ( darcsFileStore ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Time (formatTime)
import System.Locale (defaultTimeLocale)
import Data.List (sort, isPrefixOf)
#ifdef USE_MAXCOUNT
import Data.List (isInfixOf)
#endif
import System.Exit (ExitCode(..))
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath ((</>), dropFileName, addTrailingPathSeparator)

import Data.FileStore.DarcsXml (parseDarcsXML)
import Data.FileStore.Types
import Data.FileStore.Utils (withSanityCheck, hashsMatch, runShellCommand, ensureFileExists, grepSearchRepo, withVerifyDir, encodeArg)

import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B (ByteString, writeFile, null)

-- | Return a filestore implemented using the Darcs distributed revision control system
-- (<http://darcs.net/>).
darcsFileStore :: FilePath -> FileStore
darcsFileStore repo = FileStore {
    initialize      = darcsInit repo
  , save            = darcsSave repo
  , retrieve        = darcsRetrieve repo
  , delete          = darcsDelete repo
  , rename          = darcsMove repo
  , history         = darcsLog repo
  , latest          = darcsLatestRevId repo
  , revision        = darcsGetRevision repo
  , index           = darcsIndex repo
  , directory       = darcsDirectory repo
  , search          = darcsSearch repo
  , idsMatch        = const hashsMatch repo }
                   
-- | Run a darcs command and return error status, error output, standard output.  The repository
-- is used as working directory.
runDarcsCommand :: FilePath -> String -> [String] -> IO (ExitCode, String, B.ByteString)
runDarcsCommand repo command args = do
  (status, err, out) <- runShellCommand repo Nothing "darcs" (command : args)
  return (status, toString err, out)

---------------------------
-- End utility functions and types
-- Begin repository creation & modification
---------------------------

-- | Initialize a repository, creating the directory if needed.
darcsInit :: FilePath -> IO ()
darcsInit repo = do
  exists <- doesDirectoryExist repo
  when exists $ withVerifyDir repo $ throwIO RepositoryExists
  createDirectoryIfMissing True repo
  (status, err, _) <- runDarcsCommand repo "init" []
  if status == ExitSuccess
     then return ()
     else throwIO $ UnknownError $ "darcs init failed:\n" ++ err

-- | Save changes (creating the file and directory if needed), add, and commit.
darcsSave :: Contents a => FilePath -> FilePath -> Author -> Description -> a -> IO ()
darcsSave repo name author logMsg contents = do
  withSanityCheck repo ["_darcs"] name $ B.writeFile (repo </> encodeArg name) $ toByteString contents
  -- Just in case it hasn't been added yet; we ignore failures since darcs will
  -- fail if the file doesn't exist *and* if the file exists but has been added already.
  runDarcsCommand repo "add" [name]
  darcsCommit repo [name] author logMsg

-- | Commit changes to a resource.  Raise 'Unchanged' exception if there were none.
--   This is not for creating a new file; see 'darcsSave'. This is just for updating.
darcsCommit :: FilePath -> [FilePath] -> Author -> Description -> IO ()
darcsCommit repo names author logMsg = do
  let args = ["--all", "-A", (authorName author ++ " <" ++ authorEmail author ++ ">"), "-m", logMsg] ++ names
  (statusCommit, errCommit, _) <- runDarcsCommand repo "record" args
  if statusCommit == ExitSuccess
     then return ()
     else throwIO $ if null errCommit
                       then Unchanged
                       else UnknownError $ "Could not darcs record " ++ unwords names ++ "\n" ++ errCommit

-- | Change the name of a resource.
darcsMove :: FilePath -> FilePath -> FilePath -> Author -> Description -> IO ()
darcsMove repo oldName newName author logMsg = do
  withSanityCheck repo ["_darcs"] newName $ do
    (statusAdd, _, _) <- runDarcsCommand repo "add" [dropFileName newName]
    (statusAdd', _,_) <- runDarcsCommand repo "mv" [oldName, newName]
    if statusAdd == ExitSuccess && statusAdd' == ExitSuccess
       then darcsCommit repo [oldName, newName] author logMsg
       else throwIO NotFound

-- | Delete a resource from the repository.
darcsDelete :: FilePath -> FilePath -> Author -> Description -> IO ()
darcsDelete repo name author logMsg = withSanityCheck repo ["_darcs"] name $ do
  runShellCommand repo Nothing "rm" [name]
  darcsCommit repo [name] author logMsg

---------------------------
-- End repository creation & modification
-- Begin repository & history queries
--------------------------

-- | Return list of log entries for the list of resources.
-- If list of resources is empty, log entries for all resources are returned.
darcsLog :: FilePath -> [FilePath] -> TimeRange -> Maybe Int -> IO [Revision]
darcsLog repo names (TimeRange begin end) mblimit = do
       (status, err, output) <- runDarcsCommand repo "changes" $ ["--xml-output", "--summary"] ++ names ++ opts
       if status == ExitSuccess
        then case parseDarcsXML $ toString output of
            Nothing      -> throwIO ResourceExists
            Just parsed -> return parsed
        else throwIO $ UnknownError $ "darcs changes returned error status.\n" ++ err
    where
        opts = timeOpts begin end ++ limit
        limit = case mblimit of
#ifdef USE_MAXCOUNT
                    Just lim  -> ["--max-count",show lim]
#else
                    Just _    -> []
#endif
                    Nothing   -> []
        timeOpts :: Maybe UTCTime -> Maybe UTCTime ->[String]
        timeOpts b e = case (b,e) of
                (Nothing,Nothing) -> []
                (Just b', Just e') -> from b' ++ to e'
                (Just b', Nothing) -> from b'
                (Nothing, Just e') -> to e'
                where from z = ["--match=date \"after " ++ undate z ++ "\""]
                      to z = ["--to-match=date \"before " ++ undate z ++ "\""]
                      undate = toSqlString
                      toSqlString = formatTime defaultTimeLocale "%FT%X"

-- | Get revision information for a particular revision ID, or latest revision.
darcsGetRevision :: FilePath -> RevisionId -> IO Revision
darcsGetRevision repo hash = do (_,_,output) <- runDarcsCommand repo "changes"
                                                ["--xml-output", "--summary", "--match=hash " ++ hash]
                                let hists = parseDarcsXML $ toString output
                                case hists of
                                    Nothing -> throwIO NotFound
                                    Just a  -> return $ head a

-- | Return revision ID for latest commit for a resource.
darcsLatestRevId :: FilePath -> FilePath -> IO RevisionId
darcsLatestRevId repo name = do
  ensureFileExists repo name
#ifdef USE_MAXCOUNT
  (status, err, output) <- runDarcsCommand repo "changes" ["--xml-output", "--max-count=1", name]
  when (status /= ExitSuccess && "unrecognized option" `isInfixOf` err) $ throwIO NoMaxCount
#else
  (_, _, output) <- runDarcsCommand repo "changes" ["--xml-output", name]
#endif
  let patchs = parseDarcsXML $ toString output
  case patchs of
      Nothing -> throwIO NotFound
      Just as -> if null as then throwIO NotFound else return $ revId $ head as

-- | Retrieve the contents of a resource.
darcsRetrieve :: Contents a
            => FilePath
            -> FilePath
            -> Maybe RevisionId    -- ^ @Just@ revision ID, or @Nothing@ for latest
            -> IO a
darcsRetrieve repo name mbId = do
  let opts = case mbId of
              Nothing    -> ["contents", name]
              Just revid -> ["contents", "--match=hash " ++ revid, name]
  (status, err, output) <- runDarcsCommand repo "query" opts
  if B.null output
     then do
       (_, _, out) <- runDarcsCommand repo "query" (["files", "--no-directories"] ++ opts)
       if B.null out || null (filter (== name) . getNames $ output)
         then throwIO NotFound else return ()
         else return ()
  if status == ExitSuccess
     then return $ fromByteString output
     else throwIO $ UnknownError $ "Error in darcs query contents:\n" ++ err
          
getNames :: B.ByteString -> [String]
getNames = map (drop 2) . lines . toString

-- | Get a list of all known files inside and managed by a repository.
darcsIndex :: FilePath ->IO [FilePath]
darcsIndex repo = withVerifyDir repo $ do
  (status, _errOutput, output) <- runDarcsCommand repo "query"  ["files","--no-directories"]
  if status == ExitSuccess
     then return . getNames $ output
     else return []   -- return empty list if invalid path (see gitIndex)

-- | Get a list of all resources inside a directory in the repository.
darcsDirectory :: FilePath -> FilePath -> IO [Resource]
darcsDirectory repo dir = withVerifyDir (repo </> dir) $ do
  let dir' = if null dir then "" else addTrailingPathSeparator dir
  (status1, _errOutput1, output1) <- runDarcsCommand repo "query"  ["files","--no-directories"]
  (status2, _errOutput2, output2) <- runDarcsCommand repo "query" ["files","--no-files"]
  if status1 == ExitSuccess && status2 == ExitSuccess
     then do
       let files = adhocParsing dir' . lines . toString $ output1
       -- We need to do 'drop $ length dir' + 3' because Darcs returns files like ["./foo/foobar"].
       let dirs  = adhocParsing dir' . drop 1 . lines . toString $ output2
       -- We need the drop 1 to eliminate the root directory, which appears first.
       -- Now, select the ones that are in THIS directory and convert to Resources:
       let files' = map FSFile  $ filter ('/' `notElem`) files
       let dirs'  = map FSDirectory $ filter ('/' `notElem`) dirs
       return $ sort (files' ++ dirs') 
     else return []  -- returns empty list for invalid path (see gitDirectory)
              where adhocParsing d = map (drop $ length d + 2) . filter (("." </> d) `isPrefixOf`)

-- Use the generic grep-based search of a repo.
darcsSearch :: FilePath -> SearchQuery -> IO [SearchMatch]
darcsSearch = grepSearchRepo darcsIndex
