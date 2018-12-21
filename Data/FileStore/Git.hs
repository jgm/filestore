{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}

{- |
   Module      : Data.FileStore.Git
   Copyright   : Copyright (C) 2009 John MacFarlane
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : GHC 6.10 required

   A versioned filestore implemented using git.
   Normally this module should not be imported: import
   "Data.FileStore" instead.

   It is assumed that git >= 1.7.2 is available on
   the system path.
-}

module Data.FileStore.Git
           ( gitFileStore
           )
where
import Data.FileStore.Types
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List.Split (endByOneOf)
import System.Exit
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.FileStore.Utils (withSanityCheck, hashsMatch, runShellCommand, escapeRegexSpecialChars, withVerifyDir, encodeArg)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad (when)
import System.FilePath ((</>), splitFileName)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, executable, getPermissions, setPermissions)
import Control.Exception (throwIO)
import Paths_filestore
import qualified Control.Exception as E

-- | Return a filestore implemented using the git distributed revision control system
-- (<http://git-scm.com/>).
gitFileStore :: FilePath -> FileStore
gitFileStore repo = FileStore {
    initialize        = gitInit repo
  , save              = gitSave repo 
  , retrieve          = gitRetrieve repo
  , delete            = gitDelete repo
  , rename            = gitMove repo
  , history           = gitLog repo
  , latest            = gitLatestRevId repo
  , revision          = gitGetRevision repo
  , index             = gitIndex repo
  , directory         = gitDirectory repo
  , search            = gitSearch repo 
  , idsMatch          = const hashsMatch repo
  }

-- | Run a git command and return error status, error output, standard output.  The repository
-- is used as working directory.
runGitCommand :: FilePath -> String -> [String] -> IO (ExitCode, String, B.ByteString)
runGitCommand = runGitCommandWithEnv []

-- | Run a git command with the given environment and return error status, error output, standard
-- output.  The repository is used as working directory.
runGitCommandWithEnv :: [(String, String)] -> FilePath -> String -> [String] -> IO (ExitCode, String, B.ByteString)
runGitCommandWithEnv givenEnv repo command args = do
  let env = Just ([("GIT_DIFF_OPTS", "-u100000")] ++ givenEnv)
  (status, err, out) <- runShellCommand repo env "git" (command : args)
  return (status, toString err, out)

-- | Initialize a repository, creating the directory if needed.
gitInit :: FilePath -> IO ()
gitInit repo = do
  exists <- doesDirectoryExist repo
  when exists $ withVerifyDir repo $ throwIO RepositoryExists
  createDirectoryIfMissing True repo
  (status, err, _) <- runGitCommand repo "init" []
  if status == ExitSuccess
     then do
       -- Add the post-update hook, so that changes made remotely via git
       -- will be reflected in the working directory.
       postupdatepath <- getDataFileName $ "extra" </> "post-update"
       postupdatecontents <- B.readFile postupdatepath
       let postupdatedir = repo </> ".git" </> "hooks"
       createDirectoryIfMissing True postupdatedir
       let postupdate = postupdatedir </> "post-update"
       B.writeFile postupdate postupdatecontents
       perms <- getPermissions postupdate
       setPermissions postupdate (perms {executable = True})
       -- Set up repo to allow push to current branch
       (status', err', _) <- runGitCommand repo "config" ["receive.denyCurrentBranch","ignore"]
       if status' == ExitSuccess
          then return ()
          else throwIO $ UnknownError $ "git config failed:\n" ++ err'
     else throwIO $ UnknownError $ "git-init failed:\n" ++ err 

-- | Commit changes to a resource.  Raise 'Unchanged' exception if there were
-- no changes.
gitCommit :: FilePath -> [FilePath] -> Author -> String -> IO ()
gitCommit repo names author logMsg = do
  let env = [("GIT_COMMITTER_NAME", authorName author),
             ("GIT_COMMITTER_EMAIL", authorEmail author)]
  (statusCommit, errCommit, _) <- runGitCommandWithEnv env repo "commit" $ ["--author", authorName author ++ " <" ++
                                    authorEmail author ++ ">", "-m", logMsg] ++ names
  if statusCommit == ExitSuccess
     then return ()
     else throwIO $ if null errCommit
                       then Unchanged
                       else UnknownError $ "Could not git commit " ++ unwords names ++ "\n" ++ errCommit

-- | Save changes (creating file and directory if needed), add, and commit.
gitSave :: Contents a => FilePath -> FilePath -> Author -> Description -> a -> IO ()
gitSave repo name author logMsg contents = do
  withSanityCheck repo [".git"] name $ B.writeFile (repo </> encodeArg name) $ toByteString contents
  (statusAdd, errAdd, _) <- runGitCommand repo "add" [name]
  if statusAdd == ExitSuccess
     then gitCommit repo [name] author logMsg
     else throwIO $ UnknownError $ "Could not git add '" ++ name ++ "'\n" ++ errAdd

isSymlink :: FilePath -> FilePath -> Maybe RevisionId -> IO Bool
isSymlink repo name revid = do
  (_, _, out) <- runGitCommand repo "ls-tree" [fromMaybe "HEAD" revid, name]
  -- see http://stackoverflow.com/questions/737673
  return $ (take 6 $ B.unpack out) == "120000"

targetContents :: Contents a => FilePath -> FilePath -> a -> IO (Maybe a)
targetContents repo linkName linkContent = do
  let (dirName, _) = splitFileName linkName
      targetName   = repo </> dirName </> (B.unpack $ toByteString linkContent)
  result <- E.try $ B.readFile targetName
  case result of
    Left (_ :: E.SomeException) -> return Nothing
    Right contents -> return $ Just (fromByteString contents)

-- | Retrieve contents from resource.
gitRetrieve :: Contents a
            => FilePath
            -> FilePath
            -> Maybe RevisionId    -- ^ @Just@ revision ID, or @Nothing@ for latest
            -> IO a
gitRetrieve repo name revid = do
  let objectName = case revid of
                        Nothing  -> "HEAD:" ++ name
                        Just rev -> rev ++ ":" ++ name
  -- Check that the object is a file (blob), not a directory (tree)
  (_, _, output) <- runGitCommand repo "cat-file" ["-t", objectName]
  when (take 4 (toString output) /= "blob") $ throwIO NotFound
  (status', err', output') <- runGitCommand repo "cat-file" ["-p", objectName]
  if status' == ExitSuccess
     then do
       isLink <- isSymlink repo name revid
       if isLink
        then do
          contents <- targetContents repo name output'
          case contents of
            -- ideal output on Nothing would be something like
            -- "broken symlink: <output'>", but I couldn't figure
            -- out the bytestring types to do that.
            -- also didn't bother trying to get the browser
            -- to display the error as text if the symlink is to some
            -- other format.
            Nothing -> return $ fromByteString output'
            Just bs -> return $ fromByteString bs
        else return $ fromByteString output'
     else throwIO $ UnknownError $ "Error in git cat-file:\n" ++ err'

-- | Delete a resource from the repository.
gitDelete :: FilePath -> FilePath -> Author -> Description -> IO ()
gitDelete repo name author logMsg = withSanityCheck repo [".git"] name $ do
  (statusAdd, errRm, _) <- runGitCommand repo "rm" [name]
  if statusAdd == ExitSuccess
     then gitCommit repo [name] author logMsg
     else throwIO $ UnknownError $ "Could not git rm '" ++ name ++ "'\n" ++ errRm

-- | Change the name of a resource.
gitMove :: FilePath -> FilePath -> FilePath -> Author -> Description -> IO ()
gitMove repo oldName newName author logMsg = do
  _ <- gitLatestRevId repo oldName   -- will throw a NotFound error if oldName doesn't exist
  (statusAdd, err, _) <- withSanityCheck repo [".git"] newName $ runGitCommand repo "mv" [oldName, newName] 
  if statusAdd == ExitSuccess
     then gitCommit repo [oldName, newName] author logMsg
     else throwIO $ UnknownError $ "Could not git mv " ++ oldName ++ " " ++ newName ++ "\n" ++ err

-- | Return revision ID for latest commit for a resource.
gitLatestRevId :: FilePath -> FilePath -> IO RevisionId
gitLatestRevId repo name = do
  (revListStatus, _, output) <- runGitCommand repo "rev-list" ["--max-count=1", "HEAD", "--", name]
  -- we need to check separately to make sure the resource hasn't been removed
  -- from the repository:
  (catStatus,_, _) <- runGitCommand repo "cat-file" ["-e", "HEAD:" ++ name]
  if revListStatus == ExitSuccess && catStatus == ExitSuccess
     then do
       let result = takeWhile (`notElem` "\n\r \t") $ toString output
       if null result
          then throwIO NotFound
          else return result
     else throwIO NotFound

-- | Get revision information for a particular revision ID, or latest revision.
gitGetRevision :: FilePath -> RevisionId -> IO Revision
gitGetRevision repo revid = do
  (status, _, output) <- runGitCommand repo "whatchanged" ["-z","--pretty=format:" ++ gitLogFormat, "--max-count=1", revid]
  if status == ExitSuccess
     then parseLogEntry $ B.drop 1 output -- drop initial \1
     else throwIO NotFound

-- | Get a list of all known files inside and managed by a repository.
gitIndex :: FilePath ->IO [FilePath]
gitIndex repo = withVerifyDir repo $ do
  (status, _err, output) <- runGitCommand repo "ls-tree" ["-r","-t","-z","HEAD"]
  if status == ExitSuccess
     then return $ mapMaybe (lineToFilename . words) . endByOneOf ['\0'] . toString $ output
     else return [] -- if error, will return empty list
                    -- note:  on a newly initialized repo, 'git ls-tree HEAD' returns an error
   where lineToFilename (_:"blob":_:rest) = Just $ unwords rest
         lineToFilename _                 = Nothing

-- | Get list of resources in one directory of the repository.
gitDirectory :: FilePath -> FilePath -> IO [Resource]
gitDirectory repo dir = withVerifyDir (repo </> dir) $ do
  (status, _err, output) <- runGitCommand repo "ls-tree" ["-z","HEAD:" ++ dir]
  if status == ExitSuccess
     then return $ map (lineToResource . words) $ endByOneOf ['\0'] $ toString output
     else return []   -- if error, this will return empty list
                      -- note:  on a newly initialized repo, 'git ls-tree HEAD:' returns an error
   where lineToResource (_:"blob":_:rest) = FSFile $ unwords rest
         lineToResource (_:"tree":_:rest) = FSDirectory $ unwords rest
         lineToResource _                 = error "Encountered an item that is neither blob nor tree in git ls-tree"

-- | Uses git-grep to search repository.  Escape regex special characters, so the pattern
-- is interpreted as an ordinary string.
gitSearch :: FilePath -> SearchQuery -> IO [SearchMatch]
gitSearch repo query = do
  let opts = ["-I","-n","--null"] ++
             ["--ignore-case" | queryIgnoreCase query] ++
             ["--all-match" | queryMatchAll query] ++
             ["--word-regexp" | queryWholeWords query]
  (status, errOutput, output) <- runGitCommand repo "grep" (opts ++
                                   concatMap (\term -> ["-e", escapeRegexSpecialChars term]) (queryPatterns query))
  case status of
     ExitSuccess   -> return $ map parseMatchLine $ lines $ toString output
     ExitFailure 1 -> return []  -- status of 1 means no matches in recent versions of git
     ExitFailure _ -> throwIO $ UnknownError $ "git grep returned error status.\n" ++ errOutput

-- Auxiliary function for searchResults
parseMatchLine :: String -> SearchMatch
parseMatchLine str =
  SearchMatch{ matchResourceName = fname
             , matchLineNumber = if not (null ln)
                                    then read ln
                                    else error $ "parseMatchLine: " ++ str
             , matchLine = cont}
    where (fname,xs) = break (== '\NUL') str
          rest = drop 1 xs 
          -- for some reason, NUL is used after line number instead of
          -- : when --match-all is passed to git-grep.
          (ln,ys) = span (`elem` ['0'..'9']) rest
          cont = drop 1 ys   -- drop : or NUL after line number

{-
-- | Uses git-diff to get a dif between two revisions.
gitDiff :: FilePath -> FilePath -> RevisionId -> RevisionId -> IO String
gitDiff repo name from to = do
  (status, _, output) <- runGitCommand repo "diff" [from, to, name]
  if status == ExitSuccess
     then return $ toString output
     else do
       -- try it without the path, since the error might be "not in working tree" for a deleted file
       (status', err', output') <- runGitCommand repo "diff" [from, to]
       if status' == ExitSuccess
          then return $ toString output'
          else throwIO $ UnknownError $ "git diff returned error:\n" ++ err'
-}

gitLogFormat :: String
gitLogFormat = "%x01%H%x00%ct%x00%an%x00%ae%x00%B%n%x00"

-- | Return list of log entries for the given time frame and list of resources.
-- If list of resources is empty, log entries for all resources are returned.
gitLog :: FilePath -> [FilePath] -> TimeRange -> Maybe Int -> IO [Revision]
gitLog repo names (TimeRange mbSince mbUntil) mblimit = do
  (status, err, output) <- runGitCommand repo "whatchanged" $
                           ["-z","--pretty=format:" ++ gitLogFormat] ++
                           (case mbSince of
                                 Just since   -> ["--since='" ++ show since ++ "'"]
                                 Nothing      -> []) ++
                           (case mbUntil of
                                 Just til   -> ["--until='" ++ show til ++ "'"]
                                 Nothing      -> []) ++
                           (case mblimit of
                                 Just lim   -> ["-n", show lim]
                                 Nothing    -> []) ++
                           ["--"] ++ names
  if status == ExitSuccess
     then parseGitLog output
     else throwIO $ UnknownError $ "git whatchanged returned error status.\n" ++ err

--
-- Parsers to parse git log into Revisions.
--

parseGitLog :: B.ByteString -> IO [Revision]
parseGitLog = mapM parseLogEntry . splitEntries

splitEntries :: B.ByteString -> [B.ByteString]
splitEntries = dropWhile B.null . B.split '\1' -- occurs just before each hash

parseLogEntry :: B.ByteString -> IO Revision
parseLogEntry entry = do
  let (rev : date' : author : email : subject : rest) = B.split '\0' entry
  date <- case B.readInteger date' of
               Just (x,_) -> return x
               Nothing    -> throwIO $ UnknownError $ "Could not read date"
  changes <- parseChanges $ takeWhile (not . B.null) rest
  return Revision {
              revId          = toString rev
            , revDateTime    = posixSecondsToUTCTime $ realToFrac date
            , revAuthor      = Author{ authorName = toString author
                                     , authorEmail = toString email }
            , revDescription = toString $ stripTrailingNewlines subject
            , revChanges     = changes }

stripTrailingNewlines :: B.ByteString -> B.ByteString
stripTrailingNewlines = B.reverse . B.dropWhile (=='\n') . B.reverse

-- | This function converts the git "log" %B (raw body) format into a
-- list of Change items (e.g. `Added FilePath`, `Modified FilePath`,
-- or `Deleted FilePath`).  The raw body format is normally pairs of
-- ByteStrings, like:
--
--    ":000000 100644 0000000... 9cf8bba... A", "path/to/file.foo"
--
-- where the last letter of the first element is the type of change.
-- Git can track renames however, and those are noted by a triple of
-- ByteStrings; for example:
--
--   ":100644 100644 6c2c6e2... d333ad0... R063",
--   "old/file/path/name.foo",
--   "new/file/path/newname.bar"
--
-- Since filestore does not track renames, these are converted to
-- a remove of the first file and an add of the second.
--
-- n.b. without reading git sources, it's not clear what the raw body
-- format details are; specifically, the three digits following the R
-- are ignored.
parseChanges :: [B.ByteString] -> IO [Change]
parseChanges (x:y:zs) = do
  when (B.null x) $ pcErr "found empty change description"
  let changeType = B.head $ last $ B.words x
  let file' = toString y
  if changeType == 'R'
  then parseChanges (tail zs) >>=
       return . (++) (Deleted file' : Added (toString $ head zs) : [])
  else
      do next <- case changeType of
                   'A'  -> return $ Added file'
                   'M'  -> return $ Modified file'
                   'D'  -> return $ Deleted file'
                   _    -> pcErr ("found unknown changeType '" ++
                                  (show changeType) ++
                                  "' in: " ++ (show x) ++
                                  " on " ++ (show y))
         rest <- parseChanges zs
         return (next:rest)
parseChanges [_] =
  pcErr "encountered odd number of fields"
parseChanges [] = return []

pcErr :: forall a. String -> IO a
pcErr = throwIO . UnknownError . (++) "filestore parseChanges "
