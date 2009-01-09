{- A git instance of the abstract filestore defined in
-  Data.FileStore.  Derived from Gitit's git functions.

   (C) 2008 John MacFarlane
-}

module Data.FileStore.Git
           ( GitFileStore(..)
           , gitInit
           , gitCreate
           , gitModify
           , gitRetrieve
           , gitDelete
           , gitMove
           , gitHistory
           , gitGetRevision
           , gitIndex
           , gitSearch
           , gitDiff
           )
where
import Data.FileStore.Types
import System.Exit
import System.IO.Error (isDoesNotExistError)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.FileStore.Utils (runShellCommand, mergeContents) 
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import qualified Data.ByteString.Lazy as B
import qualified Text.ParserCombinators.Parsec as P
import Codec.Binary.UTF8.String (decodeString)
import Data.Char (chr)
import Control.Monad (liftM, when)
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import Codec.Binary.UTF8.String (encodeString)
import Control.Exception (throwIO)
import Text.Regex.Posix ((=~))

newtype GitFileStore = GitFileStore {
                          gitRepoPath :: FilePath
                          } deriving (Read, Eq, Show)

-- | Run git command and return error status, error output, standard output.  The repository
-- is used as working directory.
runGitCommand :: GitFileStore -> String -> [String] -> IO (ExitCode, String, B.ByteString)
runGitCommand repo command args = do
  let env = Just [("GIT_DIFF_OPTS","-u100000")]
  (status, err, out) <- runShellCommand (gitRepoPath repo) env "git" (command : args)
  return (status, toString err, out)

gitInit :: GitFileStore -> IO ()
gitInit repo = do
  exists <- doesDirectoryExist (gitRepoPath repo)
  when exists $ throwIO $ RepositoryExists
  createDirectoryIfMissing True (gitRepoPath repo)
  (status, err, _) <- runGitCommand repo "init" []
  if status == ExitSuccess
     then return ()
     else throwIO $ UnknownError $ "git-init failed:\n" ++ err 

-- | Returns True if the revision ids match -- that is, if one
-- is a sublist of the other.  Note that git allows prefixes of complete sha1
-- hashes to be used as identifiers.
matches :: RevisionId -> RevisionId -> Bool
matches r1 r2 = r1 `isPrefixOf` r2 || r2 `isPrefixOf` r1

gitCreate :: Contents a => GitFileStore -> ResourceName -> Author -> String -> a -> IO ()
gitCreate repo name author logMsg contents = do
  let filename = gitRepoPath repo </> encodeString name
  exists <- doesFileExist filename
  when exists $ throwIO ResourceExists
  let dir' = takeDirectory filename
  createDirectoryIfMissing True dir'
  B.writeFile filename $ toByteString contents
  (statusAdd, errAdd, _) <- runGitCommand repo "add" [name]
  if statusAdd == ExitSuccess
     then gitCommit repo name (authorName author, authorEmail author) logMsg
     else throwIO $ UnknownError $ "Could not git add '" ++ name ++ "'\n" ++ errAdd

gitModify :: Contents a => GitFileStore -> ResourceName -> RevisionId -> Author -> String -> a -> IO ()
gitModify repo name originalRevId author logMsg contents = do
  latestRev <- gitGetRevision repo name Nothing
  let latestRevId = revId latestRev
  if originalRevId `matches` latestRevId
     then do
       B.writeFile (gitRepoPath repo </> encodeString name) $ toByteString contents
       gitCommit repo name (authorName author, authorEmail author) logMsg
     else do
       (conflicts, mergedText) <- gitMerge repo name originalRevId latestRevId $ toByteString contents
       throwIO $ Merged (MergeInfo latestRev conflicts mergedText)

gitMerge :: GitFileStore -> ResourceName -> RevisionId -> RevisionId -> B.ByteString -> IO (Bool, String)
gitMerge repo name originalRevId latestRevId contents = do
  originalContents <- gitRetrieve repo name (Just originalRevId)
  latestContents <- gitRetrieve repo name (Just latestRevId)
  catch (mergeContents ("edited", contents) (originalRevId, originalContents) (latestRevId, latestContents))
              (\e -> throwIO $ UnknownError $ show e)

gitCommit :: GitFileStore -> ResourceName -> (String, String) -> String -> IO ()
gitCommit repo name (author, email) logMsg = do
  (statusCommit, errCommit, _) <- runGitCommand repo "commit" ["--author", author ++ " <" ++
                                    email ++ ">", "-m", logMsg, name]
  if statusCommit == ExitSuccess
     then return ()
     else throwIO $ if null errCommit
                       then Unchanged
                       else UnknownError $ "Could not git commit '" ++ name ++ "'\n" ++ errCommit

gitRetrieve :: Contents a => GitFileStore -> ResourceName -> Maybe RevisionId -> IO a
gitRetrieve repo name Nothing = do
  -- If called with Nothing, go straight to the file system
  catch (liftM fromByteString $ B.readFile (gitRepoPath repo </> name)) $
    \e -> if isDoesNotExistError e then throwIO NotFound else throwIO e
gitRetrieve repo name mbRevId = do
  rev <- gitGetRevision repo name mbRevId
  (status, err, output) <- runGitCommand repo "cat-file" ["-p", revId rev ++ ":" ++ name]
  if status == ExitSuccess
     then return $ fromByteString output
     else throwIO $ UnknownError $ "Error in git cat-file:\n" ++ err

gitDelete :: GitFileStore -> ResourceName -> Author -> String -> IO ()
gitDelete repo name author logMsg = do
  (statusAdd, errRm, _) <- runGitCommand repo "rm" [name]
  if statusAdd == ExitSuccess
     then gitCommit repo name (authorName author, authorEmail author) logMsg
     else throwIO $ UnknownError $ "Could not git rm '" ++ name ++ "'\n" ++ errRm

gitMove :: GitFileStore -> ResourceName -> Author -> String -> IO ()
gitMove = undefined

gitHistory :: GitFileStore -> [ResourceName] -> TimeRange -> IO History
gitHistory repo names (TimeRange mbSince mbUntil) =
  liftM (concatMap logEntryToHistory) $ gitLog repo names (TimeRange mbSince mbUntil)

logEntryToHistory :: LogEntry -> History
logEntryToHistory entry =
  let names = logFiles entry
      stripTrailingNewlines = reverse . dropWhile (=='\n') . reverse
      rev = Revision {
               revId          = logRevision entry
             , revDateTime    = posixSecondsToUTCTime $ realToFrac $ (read $ logDate entry :: Integer)
             , revAuthor      = Author { authorName = logAuthor entry, authorEmail = logEmail entry }
             , revDescription = stripTrailingNewlines $ logSubject entry  }
  in  map (\name -> (name, rev)) names

-- | Return list of log entries for the given time frame and list of resources.
-- If list of resources is empty, log entries for all resources are returned.
gitLog :: GitFileStore -> [ResourceName] -> TimeRange -> IO [LogEntry]
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
-- Parsers to parse git log into LogEntry records.
--

-- | Abstract representation of a git log entry.
data LogEntry = LogEntry
  { logRevision   :: String
  , logDate       :: String
  , logAuthor     :: String
  , logEmail      :: String
  , logSubject    :: String
  , logFiles      :: [String]
  } deriving (Read, Show)

parseGitLog :: P.Parser [LogEntry]
parseGitLog = P.manyTill gitLogEntry P.eof

wholeLine :: P.GenParser Char st [Char]
wholeLine = P.manyTill P.anyChar P.newline

nonblankLine :: P.GenParser Char st [Char]
nonblankLine = P.notFollowedBy P.newline >> wholeLine

gitLogEntry :: P.Parser LogEntry
gitLogEntry = do
  P.optional (P.string "commit" >> wholeLine)  -- the commit XXX message returned by git rev-list
  rev <- nonblankLine
  date <- nonblankLine
  author <- wholeLine
  email <- wholeLine
  subject <- liftM unlines (P.manyTill wholeLine (P.eof P.<|> (P.lookAhead (P.char ':') >> return ())))
  P.spaces
  files <- P.many gitLogChange
  P.spaces
  return $ LogEntry { logRevision = rev,
                      logDate = date,
                      logAuthor = author,
                      logEmail = email,
                      logSubject = subject,
                      logFiles = map convertEncoded files }

gitLogChange :: P.Parser String
gitLogChange = do
  P.char ':'
  line <- nonblankLine
  return $ unwords $ drop 5 $ words line

gitGetRevision :: GitFileStore -> ResourceName -> Maybe RevisionId -> IO Revision
gitGetRevision repo name mbRevid = do
  let revid = fromMaybe "HEAD" mbRevid
  (status, _, output) <- runGitCommand repo "rev-list" ["--pretty=format:%h%n%ct%n%an%n%ae%n%s%n", "--max-count=1", revid, "--", name]
  if status == ExitSuccess
     then do
       resources <- case P.parse parseGitLog "" (toString output) of
                           Left err'   -> throwIO $ UnknownError $ "error parsing git log: " ++ show err'
                           Right [x]   -> return $ logEntryToHistory x{logFiles = [name]}
                           Right []    -> return []
                           Right xs    -> throwIO $ UnknownError $ "git rev-list returned more than one result: " ++ show xs
       case resources of
            xs -> case lookup name xs of
                       Just r  -> return r
                       Nothing -> throwIO NotFound
     else throwIO NotFound

gitIndex :: GitFileStore -> IO [ResourceName]
gitIndex repo = do
  (status, errOutput, output) <- runGitCommand repo "ls-files" []
  if status == ExitSuccess
     then return $ map convertEncoded $ lines $ toString output
     else error $ "git ls-tree returned error status.\n" ++ errOutput

-- | git ls-tree returns UTF-8 filenames in quotes, with characters octal-escaped.
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


gitSearch :: GitFileStore -> SearchQuery -> IO [SearchMatch]
gitSearch repo query = do
  let opts = ["-I","-n"] ++
             (if queryIgnoreCase query then ["--ignore-case"] else []) ++
             (if queryMatchAll query then ["--all-match"] else []) ++
             (if queryWholeWords query then ["--word-regexp"] else []) ++
             (if queryRegex query then ["--extended-regexp"] else ["--fixed-strings"])
  (status, errOutput, output) <- runGitCommand repo "grep" (opts ++
                                   concatMap (\term -> ["-e", term]) (queryPatterns query))
  if status == ExitSuccess
     then return $ map parseMatchLine $ lines $ toString output
     else error $ "git grep returned error status.\n" ++ errOutput

-- Auxiliary function for searchResults
parseMatchLine :: String -> SearchMatch
parseMatchLine str =
  let (_,_,_,[fname,_,ln,cont]) = str =~ "^(([^:]|:[^0-9])*):([0-9]*):(.*)$" :: (String, String, String, [String])
  in  SearchMatch{matchResourceName = fname, matchLineNumber = read ln, matchLine = cont}

gitDiff :: GitFileStore -> ResourceName -> RevisionId -> RevisionId -> IO String
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

