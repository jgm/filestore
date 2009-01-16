module Data.FileStore.Darcs ( darcsFileStore ) where

import Codec.Binary.UTF8.String (encodeString)
import Control.Exception (throwIO)
import Control.Monad (liftM, unless, when)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Char (isSpace)
import Data.DateTime (formatDateTime, parseDateTime)
import Data.FileStore.Types
import Data.FileStore.Utils (hashsMatch, isInsideRepo, runShellCommand)
import Data.List (intersect, nub)
import Data.Maybe (fromMaybe, fromJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeDirectory, dropFileName)
import System.IO.Error (isDoesNotExistError)
import Text.Regex.Posix ((=~))
import Text.XML.Light
import qualified Data.ByteString.Lazy as B (ByteString, readFile, writeFile)

-- | Return a filestore implemented using the Darcs distributed revision control system
-- (<http://darcs.net/>).
darcsFileStore :: FilePath -> FileStore
darcsFileStore repo = FileStore {
    fsType     = "Darcs"
  , fsPath     = Just repo
  , initialize = darcsInit repo
  , save       = darcsSave repo
  , retrieve   = darcsRetrieve repo
  , delete     = darcsDelete repo
  , rename     = darcsMove repo
  , history    = darcsLog repo
  , latest     = darcsLatestRevId repo
  , revision   = darcsGetRevision repo
  , index      = darcsIndex repo
  , search     = darcsSearch repo
  , idsMatch   = const hashsMatch repo }

-- Copied from Git.hs
parseMatchLine :: String -> SearchMatch
parseMatchLine str =
  let (fn:n:res:_) = filter (not . (==) ":") $ split (==':') str
  in  SearchMatch{matchResourceName = fn, matchLineNumber = read n, matchLine = res}

-- | Run a darcs command and return error status, error output, standard output.  The repository
-- is used as working directory.
runDarcsCommand :: FilePath -> String -> [String] -> IO (ExitCode, String, B.ByteString)
runDarcsCommand repo command args = do
  (status, err, out) <- runShellCommand repo Nothing "darcs" (command : args)
  return (status, toString err, out)

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p s = let (l,s') = break p s in l : case s' of
                                           [] -> []
                                           (r:s'') -> [r] : split p s''

parseDarcsXML :: String -> Maybe [Revision]
parseDarcsXML str = do changelog <- parseXMLDoc str
                       let patches = filterChildrenName (\(QName n _ _) -> n == "patch") changelog
                       return $ map parseIntoRevision patches

parseIntoRevision :: Element -> Revision
parseIntoRevision a = Revision { revId = hashXML a,
                                 revDateTime = date a,
                                 revAuthor = Author { authorName=authorXML a, authorEmail=emailXML a },
                                 revDescription = descriptionXML a,
                                 revChanges = changesXML a }
    where
        -- If we can't get a date from the XML, we default to the beginning of the POSIX era.
        -- This at least makes it easy for someone to filter out bad dates, as obviously no real DVCSs
        -- were in operation then. :)
        -- date :: Element -> UTCTime
        date = fromMaybe (posixSecondsToUTCTime $ realToFrac (0::Int)) . parseDateTime "%c" . dateXML

authorXML, dateXML, descriptionXML, emailXML, hashXML :: Element -> String
authorXML = snd . splitEmailAuthor . fromMaybe "" . findAttr (QName "author" Nothing Nothing)
emailXML =  fromMaybe"" . fst . splitEmailAuthor . fromMaybe "" . findAttr (QName "author" Nothing Nothing)
dateXML   = fromMaybe "" . findAttr (QName "date" Nothing Nothing)
hashXML   = fromMaybe "" . findAttr (QName "hash" Nothing Nothing)
descriptionXML = fromMaybe "" . liftM strContent . findChild (QName "name" Nothing Nothing)

changesXML :: Element -> [Change]
changesXML = analyze . filterSummary . changes

-- | Our policy is: if the input is clearly a "name \<e\@mail.com\>" input, then we return "(Just Address, Name)"
--   If there is no '<' in the input, then it clearly can't be of that format, and so we just return "(Nothing, Name)"
--
-- > splitEmailAuthor "foo bar baz@gmail.com" ~> (Nothing,"foo bar baz@gmail.com")
-- > splitEmailAuthor "foo bar <baz@gmail.com>" ~> (Just "baz@gmail.com","foo bar")
splitEmailAuthor :: String -> (Maybe String, String)
splitEmailAuthor x = if '<' `elem` x then (Just (tail $ init c), reverse . dropWhile isSpace $ reverse b)
                                     else (Nothing,x)
    -- Will still need to trim the '<>' brackets in the email, and whitespace at the end of name
    where (_,b,c) = x =~ "[^<]*" :: (String,String,String)

changes :: Element -> Element
changes = fromJust . findElement (QName  "summary" Nothing Nothing)

analyze :: [Element] -> [Change]
analyze s = map convert s
  where convert a
           | x == "add_directory" || x == "add_file" = Added b
           | x == "remove_file" || x == "remove_directory" = Deleted b
           | x == "added_lines"
              || x == "modify_file"
              || x == "removed_lines"
              || x == "replaced_tokens" = Modified b
           | otherwise = error "Unknown change type"
             where  x = qName . elName $ a
                    b = takeWhile (/='\n') $ dropWhile isSpace $ strContent a

filterSummary :: Element -> [Element]
filterSummary = filterElementsName (\(QName {qName = x}) -> x == "add_file"
                                || x == "add_directory"
                                || x == "remove_file"
                                || x == "remove_directory"
                                || x == "modify_file"
                                || x == "added_lines"
                                || x == "removed_lines"
                                || x == "replaced_tokens")

-- Following are for 'darcsSearch'

-- | Search multiple files with a single regexp
go :: FilePath -> [String] -> String -> IO [String]
go repo filesToCheck pattern = do (_, _, result) <- runShellCommand repo
                                               Nothing  "grep" $ ["--line-number", "-l", "-E", "-e", pattern] ++ filesToCheck
                                  let results = intersect filesToCheck $ lines $ toString result
                                  return results

-- | Search a single file with multiple regexps
go' :: [String] -> FilePath -> [String] -> String -> IO [String]
go' os repo patterns file = do res <- mapM (\x -> run file x) patterns
                               return $ nub $ concat res
      where run f p = do (_,_,r) <- runShellCommand repo Nothing "grep" $
                                        os ++ [p, f]
                         return $ lines $ toString r

---------------------------
-- End utility functions and types
-- Begin repository creation & modification
---------------------------

-- | Initialize a repository, creating the directory if needed.
darcsInit :: FilePath -> IO ()
darcsInit repo = do
  exists <- doesDirectoryExist repo
  when exists $ throwIO RepositoryExists
  createDirectoryIfMissing True repo
  (status, err, _) <- runDarcsCommand repo "init" []
  if status == ExitSuccess
     then return ()
     else throwIO $ UnknownError $ "darcs init failed:\n" ++ err

-- | Save changes (creating the file and directory if needed), add, and commit.
darcsSave :: Contents a => FilePath -> ResourceName -> Author -> String -> a -> IO ()
darcsSave repo name author logMsg contents = do
  let filename = repo </> encodeString name
  inside <- isInsideRepo repo filename
  unless inside $ throwIO IllegalResourceName
  createDirectoryIfMissing True $ takeDirectory filename
  B.writeFile filename $ toByteString contents
  -- Just in case it hasn't been added yet; we ignore failures since darcs will
  -- fail if the file doesn't exist *and* if the file exists but has been added already.
  runDarcsCommand repo "add" [name]
  darcsCommit repo [name] author logMsg

-- | Commit changes to a resource.  Raise 'Unchanged' exception if there were none.
--   This is not for creating a new file; see 'darcsSave'. This is just for updating.
darcsCommit :: FilePath -> [ResourceName] -> Author -> String -> IO ()
darcsCommit repo names author logMsg = do
  let args = ["--all", "-A", (authorName author ++ " <" ++ authorEmail author ++ ">"), "-m", logMsg] ++ names
  (statusCommit, errCommit, _) <- runDarcsCommand repo "record" args
  if statusCommit == ExitSuccess
     then return ()
     else throwIO $ if null errCommit
                       then Unchanged
                       else UnknownError $ "Could not darcs record " ++ unwords names ++ "\n" ++ errCommit

-- | Change the name of a resource.
darcsMove :: FilePath -> ResourceName -> ResourceName -> Author -> String -> IO ()
darcsMove repo oldName newName author logMsg = do
  let newPath = repo </> newName
  inside <- isInsideRepo repo newPath
  unless inside $ throwIO IllegalResourceName
  -- create destination directory if missing
  createDirectoryIfMissing True $ takeDirectory newPath
  (statusAdd,_,_) <- runDarcsCommand repo "add" [dropFileName newName]
  (statusAdd',_,_) <- runDarcsCommand repo "mv" [oldName, newName]
  if statusAdd == ExitSuccess && statusAdd' == ExitSuccess
     then darcsCommit repo [oldName, newName] author logMsg
     else throwIO NotFound

-- | Delete a resource from the repository.
darcsDelete :: FilePath -> ResourceName -> Author -> String -> IO ()
darcsDelete repo name author logMsg = do
  runShellCommand repo Nothing "rm" [name]
  darcsCommit repo [name] author logMsg

---------------------------
-- End repository creation & modification
-- Begin repository & history queries
--------------------------

-- | Return list of log entries for the list of resources.
-- If list of resources is empty, log entries for all resources are returned.
darcsLog :: FilePath -> [ResourceName] -> TimeRange -> IO [Revision]
darcsLog repo names (TimeRange begin end) = do
    let opts = timeOpts begin end
    do (status, err, output) <- runDarcsCommand repo "changes" $ ["--xml-output", "--summary"] ++ names ++ opts
       if status == ExitSuccess
        then case parseDarcsXML $ toString output of
            Nothing      -> throwIO ResourceExists
            Just parsed -> return parsed
        else throwIO $ UnknownError $ "darcs changes returned error status.\n" ++ err
    where
        timeOpts :: Maybe DateTime -> Maybe DateTime ->[String]
        timeOpts b e = case (b,e) of
                (Nothing,Nothing) -> []
                (Just b', Just e') -> from b' ++ to e'
                (Just b', Nothing) -> from b'
                (Nothing, Just e') -> to e'
                where from z = ["--from-match=date \"" ++ undate z ++ "\""]
                      to z = ["--to-match=date \"" ++ undate z ++ "\""]
                      undate = formatDateTime "%c"

-- | Get revision information for a particular revision ID, or latest revision.
darcsGetRevision :: FilePath -> RevisionId -> IO Revision
darcsGetRevision repo hash = do hists <- darcsLog repo [] (TimeRange Nothing Nothing)
                                let hist = filter (\x -> hashsMatch (revId x) hash) hists
                                let result =  if null hist then hists else hist
                                return $ head result

-- | Return revision ID for latest commit for a resource.
darcsLatestRevId :: FilePath -> ResourceName -> IO RevisionId
darcsLatestRevId repo name = do
  -- changes always succeeds, so no need to check error
  (_, _, output) <- runDarcsCommand repo "changes" ["--xml-output", "--summary", name]
  let patchs = parseDarcsXML $ toString output
  case patchs of
      Nothing -> throwIO NotFound
      Just as -> if null as then throwIO NotFound else return $ revId $ head as

-- | Retrieve the (text) contents of a resource.
darcsRetrieve :: Contents a
            => FilePath
            -> ResourceName
            -> Maybe RevisionId    -- ^ @Just@ revision ID, or @Nothing@ for latest
            -> IO a
-- If called with Nothing, go straight to the file system
darcsRetrieve repo name Nothing = do
  let filename = repo </> encodeString name
  catch (liftM fromByteString $ B.readFile filename) $
    \e -> if isDoesNotExistError e then throwIO NotFound else throwIO e
darcsRetrieve repo name (Just revid) = do
   let opts = ["contents", "--match=hash " ++ revid, name]
   print opts
   (status, err, output) <- runDarcsCommand repo "query" opts
   if status == ExitSuccess
      then return $ fromByteString output
      else throwIO $ UnknownError $ "Error in darcs query contents:\n" ++ err

-- | Get a list of all known files inside and managed by a repository.
darcsIndex :: FilePath ->IO [ResourceName]
darcsIndex repo = do
    (status, errOutput, output) <- runDarcsCommand repo "query"  ["manifest"]
    if status == ExitSuccess
    -- We need to do 'drop 2' because Darcs returns files like ["./foobar"], and
    -- the ./ is always present & always not wanted.
     then return (map (drop 2) . lines . toString $ output)
     else error $ "'darcs query manifest' returned error status.\n" ++ errOutput

-- | Uses grep to search repository.
darcsSearch :: FilePath -> SearchQuery -> IO [SearchMatch]
darcsSearch repo query = do
  let opts = ["--line-number", "--with-filename"] ++
             (if queryIgnoreCase query then ["-i"] else []) ++
             (if queryWholeWords query then ["--word-regexp"] else ["-E"])
  let regexps = queryPatterns query
  files <- darcsIndex repo
  if queryMatchAll query then do
                                  filesMatchingAllPatterns <- liftM (foldr1 intersect) $ mapM (go repo files) regexps
                                  output <- mapM (go' opts repo regexps) filesMatchingAllPatterns
                                  return $ map parseMatchLine $ concat output
   else do (_status, _errOutput, output) <-
                runShellCommand repo Nothing "grep" $ opts ++
                                                       concatMap (\term -> ["-e", term]) regexps ++
                                                       files
           let results = lines $ toString output
           return $ map parseMatchLine results

