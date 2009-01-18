{- |
   Module      : Data.FileStore.Git
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : GHC 6.10 required

   A versioned filestore implemented using git.
   Normally this module should not be imported: import
   "Data.FileStore" instead.
-}

module Data.FileStore.Git
           ( gitFileStore
           )
where
import Data.FileStore.Types
import System.Exit
import System.IO.Error (isDoesNotExistError)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.FileStore.Utils (hashsMatch, isInsideRepo, runShellCommand, escapeRegexSpecialChars) 
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B
import qualified Text.ParserCombinators.Parsec as P
import Codec.Binary.UTF8.String (decodeString)
import Data.Char (chr)
import Control.Monad (liftM, when)
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import Codec.Binary.UTF8.String (encodeString)
import Control.Exception (throwIO)
import Control.Monad (unless)
import Text.Regex.Posix ((=~))
import System.Directory (getPermissions, setPermissions, executable)
import Paths_filestore

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
  , search            = gitSearch repo 
  , idsMatch          = const hashsMatch repo
  }

-- | Run a git command and return error status, error output, standard output.  The repository
-- is used as working directory.
runGitCommand :: FilePath -> String -> [String] -> IO (ExitCode, String, B.ByteString)
runGitCommand repo command args = do
  let env = Just [("GIT_DIFF_OPTS","-u100000")]
  (status, err, out) <- runShellCommand repo env "git" (command : args)
  return (status, toString err, out)

-- | Initialize a repository, creating the directory if needed.
gitInit :: FilePath -> IO ()
gitInit repo = do
  exists <- doesDirectoryExist repo
  when exists $ throwIO RepositoryExists
  createDirectoryIfMissing True repo
  (status, err, _) <- runGitCommand repo "init" []
  if status == ExitSuccess
     then do
       -- Add the post-update hook, so that changes made remotely via git
       -- will be reflected in the working directory.
       postupdatepath <- getDataFileName $ "extra" </> "post-update"
       postupdatecontents <- B.readFile postupdatepath
       let postupdate = repo </> ".git" </> "hooks" </> "post-update"
       B.writeFile postupdate postupdatecontents
       perms <- getPermissions postupdate
       setPermissions postupdate (perms {executable = True})
       return ()
     else throwIO $ UnknownError $ "git-init failed:\n" ++ err 

-- | Commit changes to a resource.  Raise 'Unchanged' exception if there were
-- no changes.
gitCommit :: FilePath -> [ResourceName] -> Author -> String -> IO ()
gitCommit repo names author logMsg = do
  (statusCommit, errCommit, _) <- runGitCommand repo "commit" $ ["--author", authorName author ++ " <" ++
                                    authorEmail author ++ ">", "-m", logMsg] ++ names
  if statusCommit == ExitSuccess
     then return ()
     else throwIO $ if null errCommit
                       then Unchanged
                       else UnknownError $ "Could not git commit " ++ unwords names ++ "\n" ++ errCommit

-- | Save changes (creating file and directory if needed), add, and commit.
gitSave :: Contents a => FilePath -> ResourceName -> Author -> String -> a -> IO ()
gitSave repo name author logMsg contents = do
  let filename = repo </> encodeString name
  inside <- isInsideRepo repo filename
  unless inside $ throwIO IllegalResourceName
  createDirectoryIfMissing True $ takeDirectory filename
  B.writeFile filename $ toByteString contents
  (statusAdd, errAdd, _) <- runGitCommand repo "add" [name]
  if statusAdd == ExitSuccess
     then gitCommit repo [name] author logMsg
     else throwIO $ UnknownError $ "Could not git add '" ++ name ++ "'\n" ++ errAdd

-- | Retrieve contents from resource.
gitRetrieve :: Contents a
            => FilePath
            -> ResourceName
            -> Maybe RevisionId    -- ^ @Just@ revision ID, or @Nothing@ for latest
            -> IO a
gitRetrieve repo name Nothing = do
  -- If called with Nothing, go straight to the file system
  let filename = repo </> encodeString name
  catch (liftM fromByteString $ B.readFile filename) $
    \e -> if isDoesNotExistError e then throwIO NotFound else throwIO e
gitRetrieve repo name (Just revid) = do
  (status, err, output) <- runGitCommand repo "cat-file" ["-p", revid ++ ":" ++ name]
  if status == ExitSuccess
     then return $ fromByteString output
     else throwIO $ UnknownError $ "Error in git cat-file:\n" ++ err

-- | Delete a resource from the repository.
gitDelete :: FilePath -> ResourceName -> Author -> String -> IO ()
gitDelete repo name author logMsg = do
  (statusAdd, errRm, _) <- runGitCommand repo "rm" [name]
  if statusAdd == ExitSuccess
     then gitCommit repo [name] author logMsg
     else throwIO $ UnknownError $ "Could not git rm '" ++ name ++ "'\n" ++ errRm

-- | Change the name of a resource.
gitMove :: FilePath -> ResourceName -> ResourceName -> Author -> String -> IO ()
gitMove repo oldName newName author logMsg = do
  gitLatestRevId repo oldName   -- will throw a NotFound error if oldName doesn't exist
  let newPath = repo </> encodeString newName
  inside <- isInsideRepo repo newPath
  unless inside $ throwIO IllegalResourceName
  -- create destination directory if missing
  createDirectoryIfMissing True $ takeDirectory newPath
  (statusAdd, err, _) <- runGitCommand repo "mv" [oldName, newName]
  if statusAdd == ExitSuccess
     then gitCommit repo [oldName, newName] author logMsg
     else throwIO $ UnknownError $ "Could not git mv " ++ oldName ++ " " ++ newName ++ "\n" ++ err

-- | Return revision ID for latest commit for a resource.
gitLatestRevId :: FilePath -> ResourceName -> IO RevisionId
gitLatestRevId repo name = do
  (status, _, output) <- runGitCommand repo "rev-list" ["--max-count=1", "HEAD", "--", name]
  if status == ExitSuccess
     then do
       let result = takeWhile (`notElem` "\n\r \t") $ toString output
       if null result
          then throwIO NotFound
          else return result
     else throwIO NotFound

-- | Get revision information for a particular revision ID, or latest revision.
gitGetRevision :: FilePath -> RevisionId -> IO Revision
gitGetRevision repo revid = do
  (status, _, output) <- runGitCommand repo "whatchanged" ["--pretty=format:%h%n%ct%n%an%n%ae%n%s%n", "--max-count=1", revid]
  if status == ExitSuccess
     then case P.parse parseGitLog "" (toString output) of
                 Left err'   -> throwIO $ UnknownError $ "error parsing git log: " ++ show err'
                 Right [r]   -> return r
                 Right []    -> throwIO NotFound
                 Right xs    -> throwIO $ UnknownError $ "git rev-list returned more than one result: " ++ show xs
     else throwIO NotFound

-- | Get list of files in repository.
gitIndex :: FilePath -> IO [ResourceName]
gitIndex repo = do
  (status, errOutput, output) <- runGitCommand repo "ls-files" []
  if status == ExitSuccess
     then return $ map convertEncoded $ lines $ toString output
     else error $ "git ls-files returned error status.\n" ++ errOutput

-- | git ls-files returns UTF-8 filenames in quotes, with characters octal-escaped.
-- like this: "\340\244\226.page"
-- This function decodes these.
convertEncoded :: String -> String
convertEncoded s =
  case P.parse pEncodedString s s of
    Left _    -> s
    Right res -> res

pEncodedString :: P.GenParser Char st [Char]
pEncodedString = do
  P.char '"'
  res <- P.many1 (pOctalChar P.<|> P.anyChar)
  if last res == '"'
     then return $ decodeString $ init res
     else fail "No ending quotation mark."

pOctalChar :: P.GenParser Char st Char
pOctalChar = P.try $ do
  P.char '\\'
  ds <- P.count 3 (P.oneOf "01234567")
  let num = read $ "0o" ++ ds
  return $ chr num

-- | Uses git-grep to search repository.  Escape regex special characters, so the pattern
-- is interpreted as an ordinary string.
gitSearch :: FilePath -> SearchQuery -> IO [SearchMatch]
gitSearch repo query = do
  let opts = ["-I","-n"] ++
             ["--ignore-case" | queryIgnoreCase query] ++
             ["--all-match" | queryMatchAll query] ++
             ["--word-regexp" | queryWholeWords query]
  (status, errOutput, output) <- runGitCommand repo "grep" (opts ++
                                   concatMap (\term -> ["-e", escapeRegexSpecialChars term]) (queryPatterns query))
  if status == ExitSuccess
     then return $ map parseMatchLine $ lines $ toString output
     else error $ "git grep returned error status.\n" ++ errOutput

-- Auxiliary function for searchResults
parseMatchLine :: String -> SearchMatch
parseMatchLine str =
  let (_,_,_,[fname,_,ln,cont]) = str =~ "^(([^:]|:[^0-9])*):([0-9]*):(.*)$" :: (String, String, String, [String])
  in  SearchMatch{matchResourceName = fname, matchLineNumber = read ln, matchLine = cont}

{-
-- | Uses git-diff to get a dif between two revisions.
gitDiff :: FilePath -> ResourceName -> RevisionId -> RevisionId -> IO String
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

-- | Return list of log entries for the given time frame and list of resources.
-- If list of resources is empty, log entries for all resources are returned.
gitLog :: FilePath -> [ResourceName] -> TimeRange -> IO [Revision]
gitLog repo names (TimeRange mbSince mbUntil) = do
  (status, err, output) <- runGitCommand repo "whatchanged" $
                           ["--pretty=format:%h%n%ct%n%an%n%ae%n%s%n"] ++
                           (case mbSince of
                                 Just since   -> ["--since='" ++ show since ++ "'"]
                                 Nothing      -> []) ++
                           (case mbUntil of
                                 Just til   -> ["--until='" ++ show til ++ "'"]
                                 Nothing      -> []) ++
                           ["--"] ++ names 
  if status == ExitSuccess
     then case P.parse parseGitLog "" (toString output) of
                Left err'    -> throwIO $ UnknownError $ "Error parsing git log.\n" ++ show err'
                Right parsed -> return parsed
     else throwIO $ UnknownError $ "git whatchanged returned error status.\n" ++ err

--
-- Parsers to parse git log into Revisions.
--

parseGitLog :: P.Parser [Revision]
parseGitLog = P.manyTill gitLogEntry P.eof

wholeLine :: P.GenParser Char st [Char]
wholeLine = P.manyTill P.anyChar P.newline

nonblankLine :: P.GenParser Char st [Char]
nonblankLine = P.notFollowedBy P.newline >> wholeLine

gitLogEntry :: P.Parser Revision
gitLogEntry = do
  rev <- nonblankLine
  date <- nonblankLine
  author <- wholeLine
  email <- wholeLine
  subject <- liftM unlines (P.manyTill wholeLine (P.eof P.<|> (P.lookAhead (P.char ':') >> return ())))
  P.spaces
  changes <- P.many gitLogChange
  P.spaces
  let stripTrailingNewlines = reverse . dropWhile (=='\n') . reverse
  return Revision {
              revId          = rev
            , revDateTime    = posixSecondsToUTCTime $ realToFrac (read date :: Integer)
            , revAuthor      = Author { authorName = author, authorEmail = email }
            , revDescription = stripTrailingNewlines subject
            , revChanges     = changes }

gitLogChange :: P.Parser Change
gitLogChange = do
  P.char ':'
  line <- nonblankLine
  let (changeType : fileWords) = drop 4 $ words line
  let file' = convertEncoded $ unwords fileWords
  case changeType of
         "A"  -> return $ Added file'
         "M"  -> return $ Modified file'
         "D"  -> return $ Deleted file'
         _    -> return $ Modified file'
