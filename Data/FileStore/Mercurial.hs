{- |
   Module      : Data.FileStore.Mercurial
   Copyright   : Copyright (C) 2009 John MacFarlane
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : GHC 6.10 required

   A versioned filestore implemented using mercurial.
   Normally this module should not be imported: import
   "Data.FileStore" instead.
-}

module Data.FileStore.Mercurial
           ( mercurialFileStore
           )
where
import Data.FileStore.Types
import Data.Maybe (fromJust)
import System.Exit
import Data.FileStore.Utils (withSanityCheck, hashsMatch, runShellCommand, withVerifyDir, grepSearchRepo) 
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B
import qualified Text.ParserCombinators.Parsec as P
import Codec.Binary.UTF8.String (encodeString)
import Data.List (nub)
import Control.Monad (when, liftM, unless)
import System.FilePath ((</>), splitDirectories, takeFileName)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import Control.Exception (throwIO)
import System.Locale (defaultTimeLocale)
import Data.Time (parseTime)
import Data.Time.Clock (UTCTime)

-- | Return a filestore implemented using the mercurial distributed revision control system
-- (<http://mercurial.selenic.com/>).
mercurialFileStore :: FilePath -> FileStore
mercurialFileStore repo = FileStore {
    initialize        = mercurialInit repo
  , save              = mercurialSave repo 
  , retrieve          = mercurialRetrieve repo
  , delete            = mercurialDelete repo
  , rename            = mercurialMove repo
  , history           = mercurialLog repo
  , latest            = mercurialLatestRevId repo
  , revision          = mercurialGetRevision repo
  , index             = mercurialIndex repo
  , directory         = mercurialDirectory repo
  , search            = mercurialSearch repo 
  , idsMatch          = const hashsMatch repo
  }

-- | Run a mercurial command and return error status, error output, standard output.  The repository
-- is used as working directory.
runMercurialCommand :: FilePath -> String -> [String] -> IO (ExitCode, String, B.ByteString)
runMercurialCommand repo command args = do
  (status, err, out) <- runShellCommand repo Nothing "hg" (command : args)
  return (status, toString err, out)

-- | Initialize a repository, creating the directory if needed.
mercurialInit :: FilePath -> IO ()
mercurialInit repo = do
  exists <- doesDirectoryExist repo
  when exists $ withVerifyDir repo $ throwIO RepositoryExists
  createDirectoryIfMissing True repo
  (status, err, _) <- runMercurialCommand repo "init" []
  if status == ExitSuccess
     then
       -- Add a hook so that changes made remotely via hg will be reflected in
       -- the working directory.  See:
       -- http://mercurial.selenic.com/wiki/FAQ#FAQ.2BAC8-CommonProblems.Any_way_to_.27hg_push.27_and_have_an_automatic_.27hg_update.27_on_the_remote_server.3F
       B.writeFile (repo </> ".hg" </> "hgrc") $
         toByteString "[hooks]\nchangegroup = hg update >&2\n"
     else throwIO $ UnknownError $ "mercurial init failed:\n" ++ err 

-- | Commit changes to a resource.  Raise 'Unchanged' exception if there were
-- no changes.
mercurialCommit :: FilePath -> [FilePath] -> Author -> String -> IO ()
mercurialCommit repo names author logMsg = do
  let email = authorEmail author
      email' = if not (null email)
                then " <" ++ email ++ ">"
                else ""
  (statusCommit, errCommit, _) <- runMercurialCommand repo "commit" $ ["--user", authorName author ++ email', "-m", logMsg] ++ names
  unless (statusCommit == ExitSuccess) $ do
     throwIO $ if null errCommit
                  then Unchanged
                  else UnknownError $ "Could not hg commit " ++ unwords names ++ "\n" ++ errCommit

-- | Save changes (creating file and directory if needed), add, and commit.
mercurialSave :: Contents a => FilePath -> FilePath -> Author -> Description -> a -> IO ()
mercurialSave repo name author logMsg contents = do
  withSanityCheck repo [".hg"] name $ B.writeFile (repo </> encodeString name) $ toByteString contents
  (statusAdd, errAdd, _) <- runMercurialCommand repo "add" ["path:" ++ name]
  if statusAdd == ExitSuccess
     then mercurialCommit repo [name] author logMsg
     else throwIO $ UnknownError $ "Could not hg add '" ++ name ++ "'\n" ++ errAdd

-- | Retrieve contents from resource.
--   Mercurial does not track directories so catting from a directory returns all files
mercurialRetrieve :: Contents a
            => FilePath
            -> FilePath
            -> Maybe RevisionId    -- ^ @Just@ revision ID, or @Nothing@ for latest
            -> IO a
mercurialRetrieve repo name revid = do
  let revname = case revid of
                        Nothing  -> "tip"
                        Just rev -> rev
  (statcheck, _, _) <- runMercurialCommand repo "locate" ["-r", revname, "-X", "glob:" ++ name </> "*", "path:" ++ name]
  when (statcheck /= ExitSuccess) $ throwIO NotFound
  (status, err, output) <- runMercurialCommand repo "cat" ["-r", revname, "-X", "glob:" ++ name </> "*", "path:" ++ name]
  if status == ExitSuccess
     then return $ fromByteString output
     else throwIO $ UnknownError $ "Error in mercurial cat:\n" ++ err

-- | Delete a resource from the repository.
mercurialDelete :: FilePath -> FilePath -> Author -> Description -> IO ()
mercurialDelete repo name author logMsg = withSanityCheck repo [".hg"] name $ do
  (statusAdd, errRm, _) <- runMercurialCommand repo "remove" ["path:" ++ name]
  if statusAdd == ExitSuccess
     then mercurialCommit repo [name] author logMsg
     else throwIO $ UnknownError $ "Could not hg rm '" ++ name ++ "'\n" ++ errRm

-- | Change the name of a resource.
mercurialMove :: FilePath -> FilePath -> FilePath -> Author -> Description -> IO ()
mercurialMove repo oldName newName author logMsg = do
  mercurialLatestRevId repo oldName   -- will throw a NotFound error if oldName doesn't exist
  (statusAdd, err, _) <- withSanityCheck repo [".hg"] newName $ runMercurialCommand repo "mv" [oldName, newName] 
  if statusAdd == ExitSuccess
     then mercurialCommit repo [oldName, newName] author logMsg
     else throwIO $ UnknownError $ "Could not hg mv " ++ oldName ++ " " ++ newName ++ "\n" ++ err

-- | Return revision ID for latest commit for a resource.
mercurialLatestRevId :: FilePath -> FilePath -> IO RevisionId
mercurialLatestRevId repo name = do
  (status, _, output) <- runMercurialCommand repo "log" ["--template", "{node}\\n", "--limit", "1", "path:" ++ name]
  if status == ExitSuccess
     then do
       let result = takeWhile (`notElem` "\n\r \t") $ toString output
       if null result
          then throwIO NotFound
          else return result
     else throwIO NotFound

-- | Get revision information for a particular revision ID, or latest revision.
mercurialGetRevision :: FilePath -> RevisionId -> IO Revision
mercurialGetRevision repo revid = do
  (status, _, output) <- runMercurialCommand repo "log" ["--template", mercurialLogFormat, "--limit", "1", "-r", revid]
  if status == ExitSuccess
     then case P.parse parseMercurialLog "" (toString output) of
                 Left err'   -> throwIO $ UnknownError $ "error parsing mercurial log: " ++ show err'
                 Right [r]   -> return r
                 Right []    -> throwIO NotFound
                 Right xs    -> throwIO $ UnknownError $ "mercurial log returned more than one result: " ++ show xs
     else throwIO NotFound

-- | Get a list of all known files inside and managed by a repository.
mercurialIndex :: FilePath ->IO [FilePath]
mercurialIndex repo = withVerifyDir repo $ do
  (status, _err, output) <- runMercurialCommand repo "manifest" ["-r", "tip"]
  if status == ExitSuccess
     then return $ lines $ toString $ output
     else return [] -- if error, will return empty list

-- | Get list of resources in one directory of the repository.  Mercurial does not store or track directories,
--   so the locate command does not return any directories.  Instead we first list all the files, then list all
--   files in subdirectories of the given directory and use that to contruct the list of directories.
mercurialDirectory :: FilePath -> FilePath -> IO [Resource]
mercurialDirectory repo dir = withVerifyDir (repo </> dir) $ do
  (status, _, output) <- runMercurialCommand repo "locate" ["-r", "tip", "glob:" ++ (dir </> "*")]
  let files = if status == ExitSuccess
                then map (FSFile . takeFileName . removePrefix dir) $ lines $ toString output
                else []
  (status2, _, output2) <- runMercurialCommand repo "locate" ["-r", "tip", "glob:" ++ (dir </> "*" </> "*")]
  let dirs = if status2 == ExitSuccess
                then map FSDirectory $ nub $ map (head . splitDirectories . removePrefix dir) $ lines $ toString output2
                else []
  return $ files ++ dirs
 where removePrefix d = drop $ length d

-- | Use generic grep to search
mercurialSearch :: FilePath -> SearchQuery -> IO [SearchMatch]
mercurialSearch = grepSearchRepo mercurialIndex

{- The following code goes not work because of a bug in mercurial.  If the final line of a file
does not end with a newline and you search for a word in the final line, hg does not display
the line from the file correctly.  In the results, the last character line is not printed.
mercurialSearch repo query = do
  let patterns = map escapeRegexSpecialChars $ queryPatterns query
      pattern = if queryWholeWords query
                  then "(\\b" ++ foldr1 (\a b -> a ++ "\\b|\\b" ++ b) patterns ++ "\\b)"
                  else "(" ++ foldr1 (\a b -> a ++ "|" ++ b) patterns ++ ")"
  (status, errOutput, output) <- runMercurialCommand repo "grep" (["--ignore-case" | queryIgnoreCase query] ++ ["-n", "-0", pattern])
  case status of
     ExitSuccess   -> do
                       putStrLn $ show output
                       case P.parse parseMercurialSearch "" (toString output) of
                        Left err'    -> throwIO $ UnknownError $ "Error parsing mercurial search results.\n" ++ show err'
                        Right parsed -> return parsed
     ExitFailure 1 -> return []  -- status of 1 means no matches
     ExitFailure _ -> throwIO $ UnknownError $ "mercurial grep returned error status.\n" ++ errOutput
-}

mercurialLogFormat :: String
mercurialLogFormat = "{node}\\n{date|rfc822date}\\n{author|person}\\n{author|email}\\n{desc}\\x00{file_adds}\\x00{file_mods}\\x00{file_dels}\\x00"

-- | Return list of log entries for the given time frame and list of resources.
-- If list of resources is empty, log entries for all resources are returned.
mercurialLog :: FilePath -> [FilePath] -> TimeRange -> IO [Revision]
mercurialLog repo names (TimeRange mbSince mbUntil) = do
  (status, err, output) <- runMercurialCommand repo "log" $ ["--template", mercurialLogFormat] ++ revOpts mbSince mbUntil ++ names
  if status == ExitSuccess
     then case P.parse parseMercurialLog "" (toString output) of
                Left err'    -> throwIO $ UnknownError $ "Error parsing mercurial log.\n" ++ show err'
                Right parsed -> return parsed
     else throwIO $ UnknownError $ "mercurial log returned error status.\n" ++ err
 where revOpts Nothing Nothing   = []
       revOpts Nothing (Just u)  = ["-d", "<" ++ show u]
       revOpts (Just s) Nothing  = ["-d", ">" ++ show s]
       revOpts (Just s) (Just u) = ["-d", show s ++ " to " ++ show u]

--
-- Parsers to parse mercurial log into Revisions.
--

parseMercurialLog :: P.Parser [Revision]
parseMercurialLog = P.manyTill mercurialLogEntry P.eof

wholeLine :: P.GenParser Char st String
wholeLine = P.manyTill P.anyChar P.newline

nonblankLine :: P.GenParser Char st String
nonblankLine = P.notFollowedBy P.newline >> wholeLine

nullStr :: P.GenParser Char st String
nullStr = P.manyTill P.anyChar (P.satisfy (=='\x00'))

mercurialLogEntry :: P.Parser Revision
mercurialLogEntry = do
  rev <- nonblankLine
  date <- nonblankLine
  author <- nonblankLine
  email <- wholeLine
  subject <- nullStr
  P.spaces
  file_add <- liftM (map Added . lines) $ nullStr
  P.spaces
  file_mod <- liftM (map Modified . lines) $ nullStr
  P.spaces
  file_del <- liftM (map Deleted . lines) $ nullStr
  P.spaces
  let stripTrailingNewlines = reverse . dropWhile (=='\n') . reverse
  return Revision {
              revId          = rev
            , revDateTime    = fromJust (parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z" date :: Maybe UTCTime)
            , revAuthor      = Author { authorName = author, authorEmail = email }
            , revDescription = stripTrailingNewlines subject
            , revChanges     = file_add ++ file_mod ++ file_del 
            }

{-
parseMercurialSearch :: P.Parser [SearchMatch]
parseMercurialSearch = P.manyTill mercurialSearchFormat P.eof

mercurialSearchFormat :: P.Parser SearchMatch
mercurialSearchFormat = do
  fname <- nullStr
  nullStr -- revision number
  lineNum <- nullStr
  txt <- nullStr
  return SearchMatch {
             matchResourceName = fname
           , matchLineNumber = read lineNum
           , matchLine = txt
           }
-}
