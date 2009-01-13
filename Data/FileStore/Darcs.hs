module Data.FileStore.Darcs (DarcsFileStore(..)) where

import Codec.Binary.UTF8.String (encodeString)
import Control.Exception (throwIO)
import Control.Monad
import Data.ByteString.Lazy.UTF8 (toString)
import Data.FileStore.Types
import Data.FileStore.Utils (runShellCommand)
import Data.List (isInfixOf, isPrefixOf)
import System.Directory (canonicalizePath)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import System.Exit
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (isDoesNotExistError)
import Text.Regex.Posix ((=~))
import qualified Data.ByteString.Lazy as B

-- | A filestore implemented using the darcs distributed revision control system
-- (<http://darcs.net/>).
newtype DarcsFileStore = DarcsFileStore {
                          darcsRepoPath :: FilePath
                          } deriving (Read, Eq, Show)
instance FileStore DarcsFileStore where
    initialize = darcsInit
    save       = darcsSave
    retrieve   = darcsRetrieve
    delete     = darcsDelete
    rename     = darcsMove
    history    = darcsLog
    latest     = darcsLatestRevId
    revision   = darcsGetRevision
    index      = darcsIndex
    diff       = darcsDiff
    idsMatch   = darcsIdsMatch
instance SearchableFileStore DarcsFileStore where
    search     = darcsSearch

-- | Run a darcs command and return error status, error output, standard output.  The repository
-- is used as working directory.
runDarcsCommand :: DarcsFileStore -> String -> [String] -> IO (ExitCode, String, B.ByteString)
runDarcsCommand repo command args = do
  (status, err, out) <- runShellCommand (darcsRepoPath repo) Nothing "darcs" (command : args)
  return (status, toString err, out)

-- ??? Couldn't this work over all backends...
isInsideRepo :: DarcsFileStore -> ResourceName -> IO Bool
isInsideRepo fs name = do
  darcsRepoPathCanon <- canonicalizePath $ darcsRepoPath fs
  filenameCanon <- canonicalizePath name
  return (darcsRepoPathCanon `isPrefixOf` filenameCanon)

-- Copied from Git.hs
parseMatchLine :: String -> SearchMatch
parseMatchLine str =
  let (_,_,_,[fname,_,ln,cont]) = str =~ "^(([^:]|:[^0-9])*):([0-9]*):(.*)$" :: (String, String, String, [String])
  in  SearchMatch{matchResourceName = fname, matchLineNumber = read ln, matchLine = cont}

-- | Match multiple terms against multiple search items.
--
-- > searchMultiple ["f", "g"] ["fbar", "gfar", "Zaptos", "Moltres"] ~> ["gfar"]
searchMultiple :: (Eq a) =>[[a]] -> [[a]] -> [[a]]
searchMultiple terms results = filter (search' terms) results
 where search' :: (Eq a) => [[a]] -> [a] -> Bool
       search' sterms result = all (\x -> x `isInfixOf` result) sterms

---------------------------
-- End utility functions and types
---------------------------

darcsLog = error "called darcsLog"
darcsLatestRevId = error "called latestRevId"
darcsGetRevision = error "called getRevision"

-- Use --unified and --store-in-memory per Orchid.
darcsDiff :: DarcsFileStore -> ResourceName -> RevisionId -> RevisionId -> IO String
darcsDiff repo file fromhash tohash = do
  (status, err, output) <- runDarcsCommand repo "diff" [file, "--unified", "--store-in-memory",
                                                        "--match 'hash " ++ fromhash ++ "'",
                                                        "--match 'hash " ++ tohash ++ "'"]
  if status == ExitSuccess
     then return $ toString output
     else throwIO $ UnknownError $ "darcs diff returned error:\n" ++ err

-- | Retrieve contents from resource.
darcsRetrieve :: Contents a
            => DarcsFileStore
            -> ResourceName
            -> Maybe RevisionId    -- ^ @Just@ revision ID, or @Nothing@ for latest
            -> IO a
-- If called with Nothing, go straight to the file system
darcsRetrieve repo name Nothing = do
  let filename = darcsRepoPath repo </> encodeString name
  catch (liftM fromByteString $ B.readFile filename) $
    \e -> if isDoesNotExistError e then throwIO NotFound else throwIO e
darcsRetrieve repo name (Just revid) = do
   (status, err, output) <- runDarcsCommand repo "query contents" ["--match 'hash " ++ revid ++ "'", name]
   if status == ExitSuccess
      then return $ fromByteString output
      else throwIO $ UnknownError $ "Error in darcs query contents:\n" ++ err

-- | Uses grep to search repository.
darcsSearch :: DarcsFileStore -> SearchQuery -> IO [SearchMatch]
darcsSearch repo query = do
  let opts = ["-i" | queryIgnoreCase query] ++
             ["--word-regexp" | queryWholeWords query]
  let regexps = queryPatterns query
  files <- darcsIndex repo
  -- We have to do something clever since grep doesn't support git-grep's --all-match option. What we do
  -- is search for the *first* term, and then we process it later in Haskell-land against all the other terms
  -- with 'searchMultiply', which operates on a list (the results of grepping for the first term) and only lets
  -- through entries which contain (infix) all our other expressions. Tho it isn't a regexp search.
  (status, errOutput, output) <-
   runShellCommand (darcsRepoPath repo) Nothing "grep" ((opts ++
                                                       concatMap (\term -> ["-e", term]) (take 1 regexps)) ++ files)
  let results = lines $ toString output
  if status == ExitSuccess
     then if queryMatchAll query then return $ map parseMatchLine $ searchMultiple regexps results
      else return $ map parseMatchLine results
     else error $ "grep returned error status.\n" ++ toString errOutput

-- copied from Git.hs
darcsIdsMatch :: DarcsFileStore -> RevisionId -> RevisionId -> Bool
darcsIdsMatch _ r1 r2 = r1 `isPrefixOf` r2 || r2 `isPrefixOf` r1

-- | Change the name of a resource.
darcsMove :: DarcsFileStore -> ResourceName -> ResourceName -> Author -> String -> IO ()
darcsMove repo oldName newName author logMsg = do
  darcsLatestRevId repo oldName   -- will throw a NotFound error if oldName doesn't exist
  let newPath = darcsRepoPath repo </> newName
  inside <- isInsideRepo repo newPath
  unless inside $ throwIO IllegalResourceName
  -- create destination directory if missing
  createDirectoryIfMissing True $ takeDirectory newPath
  (statusAdd, err, _) <- runDarcsCommand repo "mv" [oldName, newName]
  if statusAdd == ExitSuccess
     then darcsCommit repo [oldName, newName] author logMsg
     else throwIO $ UnknownError $ "Could not darcs mv " ++ oldName ++ " " ++ newName ++ "\n" ++ err

-- | Delete a resource from the repository.
darcsDelete :: DarcsFileStore -> ResourceName -> Author -> String -> IO ()
darcsDelete repo name author logMsg = do
  runShellCommand (darcsRepoPath repo) Nothing "rm" [name]
  darcsCommit repo [name] author logMsg

-- | Commit changes to a resource.  Raise 'Unchanged' exception if there were
-- no changes.
darcsCommit :: DarcsFileStore -> [ResourceName] -> Author -> String -> IO ()
darcsCommit repo names author logMsg = do
  let args = ["--all", "-A", show (authorName author ++ " <" ++ authorEmail author ++ ">"), "-m", logMsg] ++ names
  (statusCommit, errCommit, _) <- runDarcsCommand repo "record" args
  if statusCommit == ExitSuccess
     then return ()
     else throwIO $ if null errCommit
                       then Unchanged
                       else UnknownError $ "Could not darcs record " ++ unwords names ++ "\n" ++ errCommit

-- | Save changes (creating file and directory if needed), add, and commit.
darcsSave :: Contents a => DarcsFileStore -> ResourceName -> Author -> String -> a -> IO ()
darcsSave repo name author logMsg contents = do
  let filename = darcsRepoPath repo </> encodeString name
  inside <- isInsideRepo repo filename
  unless inside $ throwIO IllegalResourceName
  createDirectoryIfMissing True $ takeDirectory filename
  B.writeFile filename $ toByteString contents
  (statusAdd, errAdd, _) <- runDarcsCommand repo "add" [name]
  if statusAdd == ExitSuccess
     then darcsCommit repo [name] author logMsg
     else throwIO $ UnknownError $ "Could not darcs add '" ++ name ++ "'\n" ++ errAdd

darcsIndex :: DarcsFileStore ->IO [ResourceName]
darcsIndex repo = do
    (status, errOutput, output) <- runDarcsCommand repo "query"  ["manifest"]
    if status == ExitSuccess
     then return (lines . toString $ output)
     else error $ "'darcs query manifest' returned error status.\n" ++ errOutput

-- | Initialize a repository, creating the directory if needed.
darcsInit :: DarcsFileStore -> IO ()
darcsInit repo = do
  exists <- doesDirectoryExist (darcsRepoPath repo)
  when exists $ throwIO RepositoryExists
  createDirectoryIfMissing True (darcsRepoPath repo)
  (status, err, _) <- runDarcsCommand repo "init" []
  if status == ExitSuccess
     then return ()
     else throwIO $ UnknownError $ "darcs init failed:\n" ++ err

{- TODO:
gitCatFile: http://book.git-scm.com/7_browsing_git_objects.html
* ??? Some sort of history browsing with metadata about revision ID and committer
gitDiff: http://book.git-scm.com/3_comparing_commits_-_git_diff.html
* Should be handleable by 'darcs diff --index=N-M ', if we can turn two patch names into
  2 indices.
gitGetSHA1,
* Gets the SHA ID of the last revision to change a specified file. I think. I see no obvious way
  to get the name of the last patch to change a file in Darcs; 'darcs diff file' doesn't have any
  option to give you it.
gitLog
* Partially implemented. Just need to figure out how to parse the XML.
gitMergeFile
* merge-file is confusing. It's used once:
 > mergeText <- gitMergeFile (pathForPage page ++ ".edited") (pathForPage page ++ ".original") (pathForPage page ++ ".latest")
  Can we do this automatically in Darcs? I don't yet understand what 'git merge-file' does.
-}


-- -- | Return list of log entries for the given time frame and commit author.
-- -- If author is null, return entries for all authors.
-- -- TODO: finish this. Need to parse the XML *groan*
-- darcsLog :: MonadIO m => String -> [String] -> m [LogEntry]
-- darcsLog author files = do (_, _, output) <- runDarcsCommand "changes" $ ["--xml-output"] ++ files
--                            -- logs <- map xmlParse output
--                            return undefined

-- -- | Add and then commit file, raising errors if either step fails.
-- darcsCommit :: MonadIO m => FilePath -> (String, String) -> String -> m ()
-- darcsCommit file (author, email) logMsg = do
--   (statusAdd, errAdd, _) <- runDarcsCommand "add" [file]
--   if statusAdd == ExitSuccess
--      then do (statusCommit, errCommit, _) <- runDarcsCommand "record" ["-A", author ++ " <" ++
--                                                email ++ ">", "-m", logMsg]
--              if statusCommit == ExitSuccess
--                 then return ()
--                 else unless (null errCommit) $ error $ "Could not darcs record " ++ file ++ "\n" ++ errCommit
--      else error $ "Could not darcs add " ++ file ++ "\n" ++ errAdd

-- -- gitGrep is just a grep on files; but *only* files which are tracked by Git.
-- -- Darcs doesn't support this directly, but 'darcs query manifest' will give us
-- -- the list of tracked files, and then we can work from there.
-- darcsGrep :: MonadIO m => [String] -> m String
-- darcsGrep patterns = do (_,_stderr,managedfiles) <- runDarcsCommand "query manifest" []
--                         repo <- liftM repositoryPath (query GetConfig)
--                         (_,results,_) <- runProgCommand repo Nothing "grep" (unwords patterns) (lines managedfiles)
--                         return results

-- darcsCatFile :: MonadIO m => String -> FilePath -> m (Maybe String)
-- darcsCatFile revision file = do
--   (status, _, output) <- runDarcsCommand "show contents" ["--patch=\"" ++ show revision ++ "\"", file]
--   return $ if status == ExitSuccess
--               then Just output
--               else Nothing
--
-- darcsMergeFile :: MonadIO m => FilePath -> FilePath -> FilePath -> m String
-- darcsMergeFile edited original latest = do repo <- liftM repositoryPath (query GetConfig)
--                                            mergeFile repo edited original latest
