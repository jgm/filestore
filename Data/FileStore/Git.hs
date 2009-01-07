{- A git instance of the abstract filestore defined in
-  Data.FileStore.  Derived from Gitit's git functions.

   (C) 2008 John MacFarlane
-}

module Data.FileStore.Git
           ( gitFileStore
           , module Data.FileStore
           )
where
import Data.FileStore
import System.Exit
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.FileStore.Utils (runProgCommand) 
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe (mapMaybe, isNothing, fromJust, fromMaybe)
import Data.List (nub, isPrefixOf)
import qualified Data.ByteString.Lazy as B
import qualified Text.ParserCombinators.Parsec as P
import Codec.Binary.UTF8.String (decodeString)
import Data.Char (chr)
import Control.Monad (liftM, when)
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import Codec.Binary.UTF8.String (encodeString)
import Control.Exception (throwIO)
import Text.Regex.Posix ((=~))

gitFileStore :: FilePath   -- ^ directory containing the git repo
             -> FileStore
gitFileStore repo =
  FileStore {
    create     = gitCreate repo
  , modify     = gitModify repo
  , retrieve   = gitRetrieve repo
  , delete     = gitDelete repo
  , move       = gitMove repo
  , history    = gitHistory repo
  , latest     = gitLatest repo
  , index      = gitIndex repo
  , search     = gitSearch repo
  , diff       = gitDiff repo
  }

-- | Run git command and return error status, error output, standard output.  The repository
-- is used as working directory.
runGitCommand :: FilePath -> String -> [String] -> IO (ExitCode, String, B.ByteString)
runGitCommand repo command args = do
  let env = Just [("GIT_DIFF_OPTS","-u100000")]
  (status, err, out) <- runProgCommand repo env "git" command args
  return (status, toString err, out)

-- | Returns True if the revision ids match -- that is, if one
-- is a sublist of the other.  Note that git allows prefixes of complete sha1
-- hashes to be used as identifiers.
matches :: RevisionId -> RevisionId -> Bool
matches r1 r2 = r1 `isPrefixOf` r2 || r2 `isPrefixOf` r1

gitCreate :: Contents a => FilePath -> ResourceName -> Author -> String -> a -> IO ()
gitCreate repo name author logMsg contents = do
  let filename = repo </> encodeString name
  exists <- doesFileExist filename
  when exists $ throwIO AlreadyExists
  let dir' = takeDirectory filename
  createDirectoryIfMissing True dir'
  B.writeFile filename $ toByteString contents
  (statusAdd, errAdd, _) <- runGitCommand repo "add" [name]
  if statusAdd == ExitSuccess
     then gitCommit repo name (authorName author, authorEmail author) logMsg
     else throwIO $ UnknownError $ "Could not git add '" ++ name ++ "'\n" ++ errAdd

gitModify :: Contents a => FilePath -> ResourceName -> RevisionId -> Author -> String -> a -> IO ()
gitModify repo name originalRevId author logMsg contents = do
  mbLatestRev <- gitLatest repo name
  when (isNothing mbLatestRev) $ throwIO NotFound
  let latestRev = fromJust mbLatestRev
  let latestRevId = revId latestRev
  if originalRevId `matches` latestRevId
     then do
       B.writeFile (repo </> encodeString name) $ toByteString contents
       gitCommit repo name (authorName author, authorEmail author) logMsg
     else do
       (rev, conflicts, mergedText) <- gitMerge repo name originalRevId latestRevId $ toByteString contents
       throwIO $ Merged (MergeInfo rev conflicts mergedText)

gitMerge :: FilePath -> ResourceName -> RevisionId -> RevisionId -> B.ByteString -> IO (Revision, Bool, String)
gitMerge repo name originalRevId latestRevId contents = do
  originalRes <- gitRetrieve repo name (Just originalRevId)
  latestRes   <- gitRetrieve repo name (Just latestRevId)
  case (originalRes, latestRes) of
       (Left err, _)                  -> return err
       (_, Left err)                  -> return err
       (Right (_, originalContents), Right (latestRev, latestContents)) -> do
          let [editedTmp, originalTmp, latestTmp] = map (encodeString name ++) [".edited",".original",".latest"]
          B.writeFile (repo </> editedTmp)  contents
          B.writeFile (repo </> originalTmp) originalContents
          B.writeFile (repo </> latestTmp) latestContents
          (conflicts, mergedText) <- gitMergeFile repo editedTmp originalTmp latestTmp
          mapM removeFile $ map (repo </>) [editedTmp, originalTmp, latestTmp]
          if conflicts == -1 -- error
             then return $ UnknownError $ "Error in git merge-file: " ++ mergedText
             else return $ Merged latestRev (conflicts > 0) mergedText

gitCommit :: FilePath -> ResourceName -> (String, String) -> String -> IO ()
gitCommit repo name (author, email) logMsg = do
  (statusCommit, errCommit, _) <- runGitCommand repo "commit" ["--author", author ++ " <" ++
                                    email ++ ">", "-m", logMsg, name]
  if statusCommit == ExitSuccess
     then return ()
     else throwIO $ if null errCommit
                       then Unchanged
                       else UnknownError $ "Could not git commit '" ++ name ++ "'\n" ++ errCommit

-- | returns (n, s), where s is merged text and n is number of conflicts, or -1 for error.
gitMergeFile :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Int, String)
gitMergeFile repo edited original latest' = do
  (status, err, out) <- runGitCommand repo "merge-file" ["--stdout", edited, original, latest']
  return $ case status of
               ExitSuccess             -> (0, toString out)
               ExitFailure n | n >= 0  -> (n, toString out)
               _                       -> (-1, err)
        
gitRetrieve :: Contents a => FilePath -> ResourceName -> Maybe RevisionId -> IO (Revision, a)
gitRetrieve repo name mbRevId = do
  let revid = fromMaybe "HEAD" mbRevId
  mbRevision <- gitGetRevision repo name revid
  case mbRevision of
       Nothing       -> throwIO NotFound
       Just revision -> do
          (status, err, output) <- runGitCommand repo "cat-file" ["-p", revid ++ ":" ++ name]
          if status == ExitSuccess
             then return (revision, fromByteString output)
             else throwIO $ UnknownError $ "Error in git cat-file:\n" ++ err

gitDelete :: FilePath -> ResourceName -> Author -> String -> IO ()
gitDelete repo name author logMsg = do
  (statusAdd, errRm, _) <- runGitCommand repo "rm" [name]
  if statusAdd == ExitSuccess
     then gitCommit repo name (authorName author, authorEmail author) logMsg
     else throwIO $ UnknownError $ "Could not git rm '" ++ name ++ "'\n" ++ errRm


gitMove :: FilePath -> ResourceName -> Author -> String -> IO ()
gitMove = undefined

gitHistory :: FilePath -> [ResourceName] -> TimeRange -> IO History
gitHistory repo names (TimeRange mbSince mbUntil) = liftM (concatMap logEntryToHistory)
  $ gitLog repo names (TimeRange mbSince mbUntil)

logEntryToHistory :: LogEntry -> History
logEntryToHistory entry =
  let names = logFiles entry
      stripTrailingNewlines = reverse . dropWhile (=='\n') . reverse
      revision = Revision {
                   revId          = logRevision entry
                 , revDateTime    = posixSecondsToUTCTime $ realToFrac $ (read $ logDate entry :: Integer)
                 , revAuthor      = Author { authorName = logAuthor entry, authorEmail = logEmail entry }
                 , revDescription = stripTrailingNewlines $ logSubject entry  }
  in  map (\name -> (name, revision)) names

-- | Return list of log entries for the given time frame and list of resources.
-- If list of resources is empty, log entries for all resources are returned.
gitLog :: FilePath -> [ResourceName] -> TimeRange -> IO [LogEntry]
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

-- | Return SHA1 hash of last commit for filename.
gitLatest :: FilePath -> ResourceName -> IO (Maybe Revision)
gitLatest repo name = gitGetRevision repo name "HEAD"

gitGetRevision :: FilePath -> ResourceName -> RevisionId -> IO (Maybe Revision)
gitGetRevision repo name revid = do
  (status, _, output) <- runGitCommand repo "rev-list" ["--pretty=format:%h%n%ct%n%an%n%ae%n%s%n", "--max-count=1", revid, "--", name]
  if status == ExitSuccess
     then do
       let resources = case P.parse parseGitLog "" (toString output) of
                               Left err'   -> error $ show err'
                               Right [x]   -> logEntryToHistory x{logFiles = [name]}
                               Right []    -> []
                               Right xs    -> error $ "git rev-list returned more than one result: " ++ show xs
       case resources of
            xs -> case lookup name xs of
                       Just r  -> return $ Just r
                       Nothing -> return Nothing
     else return Nothing

gitIndex :: FilePath -> IO [ResourceName]
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


gitSearch :: FilePath -> [String] -> IO [(ResourceName, [Integer])]
gitSearch repo patterns = do
  (status, errOutput, output) <- runGitCommand repo "grep" (["-I", "-n", "--all-match", "--ignore-case", "--word-regexp"] ++
                                   concatMap (\term -> ["-e", term]) patterns)
  if status == ExitSuccess
     then let matchLines = map parseMatchLine $ lines $ toString output
              matchedFiles = nub $ map fst matchLines
              matches' = map (\f -> (f, mapMaybe (\(a,b) -> if a == f then Just b else Nothing) matchLines)) matchedFiles
          in  return matches'
     else error $ "git grep returned error status.\n" ++ errOutput

-- Auxiliary function for searchResults
parseMatchLine :: String -> (String, Integer)
parseMatchLine str =
  let (_,_,_,[fname,_,page]) = str =~ "(([^:]|:[^0-9])*):([0-9]*):" :: (String, String, String, [String])
  in  (fname, read page)

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

