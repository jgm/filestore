{- |
   Module      : Data.FileStore
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
           ( GitFileStore(..) )
where
import Data.FileStore.Types
import System.Exit
import System.IO.Error (isDoesNotExistError)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.FileStore.Utils (runShellCommand) 
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Text.ParserCombinators.Parsec as P
import Codec.Binary.UTF8.String (decodeString)
import Data.Char (chr)
import Control.Monad (liftM, when)
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import Codec.Binary.UTF8.String (encodeString)
import Control.Exception (throwIO)
import Text.Regex.Posix ((=~))
import Data.List (isPrefixOf)

-- | A filestore implemented using the git distributed revision control system
-- (<http://git-scm.com/>).
newtype GitFileStore = GitFileStore {
                          gitRepoPath :: FilePath
                          } deriving (Read, Eq, Show)

instance FileStore GitFileStore where
    initialize = gitInit
    save       = gitSave
    retrieve   = gitRetrieve
    delete     = gitDelete
    move       = gitMove
    history    = gitHistory
    revision   = gitGetRevision
    index      = gitIndex
    diff       = gitDiff
    idsMatch   = gitIdsMatch
    -- modify, create: defaults

instance SearchableFileStore GitFileStore where 
    search     = gitSearch

-- | Run a git command and return error status, error output, standard output.  The repository
-- is used as working directory.
runGitCommand :: GitFileStore -> String -> [String] -> IO (ExitCode, String, B.ByteString)
runGitCommand repo command args = do
  let env = Just [("GIT_DIFF_OPTS","-u100000")]
  (status, err, out) <- runShellCommand (gitRepoPath repo) env "git" (command : args)
  return (status, toString err, out)

-- | Initialize a repository, creating the directory if needed.
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
gitIdsMatch :: GitFileStore -> RevisionId -> RevisionId -> Bool
gitIdsMatch _ r1 r2 = r1 `isPrefixOf` r2 || r2 `isPrefixOf` r1

-- | Commit changes to a resource.  Raise 'Unchanged' exception if there were
-- no changes.
gitCommit :: GitFileStore -> ResourceName -> Author -> String -> IO ()
gitCommit repo name author logMsg = do
  (statusCommit, errCommit, _) <- runGitCommand repo "commit" ["--author", authorName author ++ " <" ++
                                    authorEmail author ++ ">", "-m", logMsg, name]
  if statusCommit == ExitSuccess
     then return ()
     else throwIO $ if null errCommit
                       then Unchanged
                       else UnknownError $ "Could not git commit '" ++ name ++ "'\n" ++ errCommit

-- | Save changes (creating file and directory if needed), add, and commit.
gitSave :: Contents a => GitFileStore -> ResourceName -> Author -> String -> a -> IO ()
gitSave repo name author logMsg contents = do
  let filename = gitRepoPath repo </> encodeString name
  createDirectoryIfMissing True $ takeDirectory filename
  B.writeFile filename $ toByteString contents
  (statusAdd, errAdd, _) <- runGitCommand repo "add" [name]
  if statusAdd == ExitSuccess
     then gitCommit repo name author logMsg
     else throwIO $ UnknownError $ "Could not git add '" ++ name ++ "'\n" ++ errAdd

-- | Retrieve contents from resource.
gitRetrieve :: Contents a
            => GitFileStore
            -> ResourceName
            -> Maybe RevisionId    -- ^ @Just@ revision ID, or @Nothing@ for latest
            -> IO a
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

-- | Delete a resource from the repository.
gitDelete :: GitFileStore -> ResourceName -> Author -> String -> IO ()
gitDelete repo name author logMsg = do
  (statusAdd, errRm, _) <- runGitCommand repo "rm" [name]
  if statusAdd == ExitSuccess
     then gitCommit repo name author logMsg
     else throwIO $ UnknownError $ "Could not git rm '" ++ name ++ "'\n" ++ errRm

gitMove :: GitFileStore -> ResourceName -> ResourceName -> Author -> String -> IO ()
gitMove = undefined

gitHistory :: GitFileStore -> [ResourceName] -> TimeRange -> IO [Revision]
gitHistory repo names (TimeRange mbSince mbUntil) =
  gitLog repo names (TimeRange mbSince mbUntil)

gitGetRevision :: GitFileStore -> ResourceName -> Maybe RevisionId -> IO Revision
gitGetRevision repo name mbRevid = do
  let revid = fromMaybe "HEAD" mbRevid
  (status, _, output) <- runGitCommand repo "rev-list" ["--pretty=format:%h%n%ct%n%an%n%ae%n%s%n", "--max-count=1", revid, "--", name]
  if status == ExitSuccess
     then case P.parse parseGitLog "" (toString output) of
                 Left err'   -> throwIO $ UnknownError $ "error parsing git log: " ++ show err'
                 Right [r]   -> return r
                 Right []    -> throwIO NotFound
                 Right xs    -> throwIO $ UnknownError $ "git rev-list returned more than one result: " ++ show xs
     else throwIO NotFound

gitIndex :: GitFileStore -> IO [ResourceName]
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

-- | Return list of log entries for the given time frame and list of resources.
-- If list of resources is empty, log entries for all resources are returned.
gitLog :: GitFileStore -> [ResourceName] -> TimeRange -> IO [Revision]
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
  P.optional (P.string "commit" >> wholeLine)  -- the commit XXX message returned by git rev-list
  rev <- nonblankLine
  date <- nonblankLine
  author <- wholeLine
  email <- wholeLine
  subject <- liftM unlines (P.manyTill wholeLine (P.eof P.<|> (P.lookAhead (P.char ':') >> return ())))
  P.spaces
  files <- P.many gitLogChange
  P.spaces
  let stripTrailingNewlines = reverse . dropWhile (=='\n') . reverse
  return $ Revision {
              revId          = rev
            , revDateTime    = posixSecondsToUTCTime $ realToFrac $ (read $ date :: Integer)
            , revAuthor      = Author { authorName = author, authorEmail = email }
            , revDescription = stripTrailingNewlines $ subject
            , revModified    = map convertEncoded files }

gitLogChange :: P.Parser String
gitLogChange = do
  P.char ':'
  line <- nonblankLine
  return $ unwords $ drop 5 $ words line

