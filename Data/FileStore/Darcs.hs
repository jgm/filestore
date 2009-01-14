module Data.FileStore.Darcs (DarcsFileStore(..)) where

import Data.Char
import Text.Regex.Posix
import Data.Time.Clock.POSIX
import Data.Maybe
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
import System.FilePath ((</>), takeDirectory, dropFileName)
import System.IO.Error (isDoesNotExistError)
import qualified Data.ByteString.Lazy as B
import Text.XML.Light
import Data.DateTime

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
  let (fn:n:res:_) = filter (not . (==) ":") $ split (==':') str
  in  SearchMatch{matchResourceName = fn, matchLineNumber = read n, matchLine = res}

-- | Match multiple terms against multiple search items.
--
-- > searchMultiple ["f", "g"] ["fbar", "gfar", "Zaptos", "Moltres"] ~> ["gfar"]
searchMultiple :: (Eq a) =>[[a]] -> [[a]] -> [[a]]
searchMultiple terms results = filter (search' terms) results
 where search' :: (Eq a) => [[a]] -> [a] -> Bool
       search' sterms result = all (\x -> x `isInfixOf` result) sterms

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p s = let (l,s') = break p s in l : case s' of
                                           [] -> []
                                           (r:s'') -> [r] : split p s''

parseDarcsXML :: String -> Maybe [Revision]
parseDarcsXML str = do changelog <- parseXMLDoc str
                       let patches = filterChildrenName (\(QName n _ _) -> n == "patch") changelog
                       return $ map parseIntoRevision patches
                  
-- TODO: figure out how to parse the String of dateXML into a DateTime
--       Provide real input to revChanges
parseIntoRevision :: Element -> Revision
parseIntoRevision a = Revision { revId = hashXML a, 
                                 revDateTime = date a,
                                 revAuthor = Author { authorName=authorXML a, authorEmail=emailXML a }, 
                                 revDescription = descriptionXML a,
                                 revChanges = changesXML a }

authorXML, dateXML, descriptionXML, emailXML, hashXML :: Element -> String
authorXML = snd . splitEmailAuthor . fromMaybe "" . findAttr (QName "author" Nothing Nothing)
emailXML =  fromMaybe"" . fst . splitEmailAuthor . fromMaybe "" . findAttr (QName "author" Nothing Nothing)
dateXML   = fromMaybe "" . findAttr (QName "date" Nothing Nothing)
hashXML   = fromMaybe "" . findAttr (QName "hash" Nothing Nothing)
descriptionXML = fromMaybe "" . liftM strContent . findChild (QName "name" Nothing Nothing)

changesXML :: Element -> [Change]
changesXML str = analyze $ filterSummary $ changes str

-- | Our policy is: if the input is clearly a "name <e@mail.com>" input, then we return (Just Address, Name)
--   If there is no '<' in the input, then it clearly can't be of that format, and so we just return (Nothing, Name)
--
-- > splitEmailAuthor "foo bar baz@gmail.com" ~> (Nothing,"foo bar baz@gmail.com")
-- > splitEmailAuthor "foo bar <baz@gmail.com>" ~> (Just "baz@gmail.com","foo bar")
splitEmailAuthor :: String -> (Maybe String, String)
splitEmailAuthor x = if '<' `elem` x then (Just (tail $ init c), reverse . dropWhile (isSpace) $ reverse b)
                                     else (Nothing,x)
    -- Still need to trim the '<>' brackets in the email, and whitespace at the end of name
    where (_,b,c) = x =~ "[^<]*" :: (String,String,String)

-- If we can't get a date from the XML, we default to the beginning of the POSIX era.
-- This at least makes it easy for someone to filter out bad dates, as obviously no real DVCSs
-- were in operation then. :)
-- date :: Element -> UTCTime
date = fromMaybe (posixSecondsToUTCTime $ realToFrac (0::Int)) . parseDateTime "%c" . dateXML

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
                    b = takeWhile (/='\n') $ dropWhile (`elem` " \t\n\r") $ strContent a

filterSummary :: Element -> [Element]
filterSummary s = filterElementsName (\(QName {qName = x}) -> x == "add_file" 
                                || x == "add_directory" 
                                || x == "remove_file" 
                                || x == "remove_directory" 
                                || x == "modify_file" 
                                || x == "added_lines" 
                                || x == "removed_lines" 
                                || x == "replaced_tokens") s

---------------------------
-- End utility functions and types
---------------------------

-- | Get revision information for a particular revision ID, or latest revision.
darcsGetRevision :: DarcsFileStore -> RevisionId -> IO Revision
darcsGetRevision repo hash = do hists <- darcsLog repo [] (TimeRange Nothing Nothing)
                                let hist = filter (\x -> darcsIdsMatch repo (revId x) hash) hists
                                let result =  if null hist then hists else hist
                                return $ head result

-- | Return list of log entries for the list of resources.
-- If list of resources is empty, log entries for all resources are returned.
-- TODO: Actually implement TimeRange functionality?
darcsLog :: DarcsFileStore -> [ResourceName] -> TimeRange -> IO [Revision]
darcsLog repo names (TimeRange _ _) = do
  (status, err, output) <- runDarcsCommand repo "changes" $ ["--xml-output", "--summary"] ++ names
  if status == ExitSuccess
     then case parseDarcsXML $ toString output of
                Nothing      -> throwIO $ ResourceExists -- $ "Error parsing darcs changes.\n" ++ show err
                Just parsed -> return parsed
     else throwIO $ UnknownError $ "darcs changes returned error status.\n" ++ err

-- | Return revision ID for latest commit for a resource.
darcsLatestRevId :: DarcsFileStore -> ResourceName -> IO RevisionId
darcsLatestRevId repo name = do
  -- changes always succeeds
  (_, _, output) <- runDarcsCommand repo "changes" ["--xml-output", "--summary", name]
  let patchs = parseDarcsXML $ toString output
  case patchs of
      Nothing -> throwIO NotFound
      Just as -> if null as then throwIO NotFound else return $ revId $ head as

-- Use --unified and --store-in-memory per Orchid.
darcsDiff :: DarcsFileStore -> ResourceName -> RevisionId -> RevisionId -> IO String
darcsDiff repo file fromhash tohash = do
  let opts = ["--store-in-memory", "--unified",
              "--match=hash " ++ tohash ++ "",
              "--match=hash " ++ fromhash ++ "",
              file]
  (status, err, output) <- runDarcsCommand repo "diff" opts
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
   let opts = ["contents", "--match=hash " ++ revid, name]
   (status, err, output) <- runDarcsCommand repo "query" opts
   if status == ExitSuccess
      then return $ fromByteString output
      else throwIO $ UnknownError $ "Error in darcs query contents:\n" ++ err

-- | Uses grep to search repository.
darcsSearch :: DarcsFileStore -> SearchQuery -> IO [SearchMatch]
darcsSearch repo query = do
  let opts = ["--line-number"] ++
             (if queryIgnoreCase query then ["-i"] else []) ++
             (if queryWholeWords query then ["--word-regexp"] else [])
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
  let newPath = darcsRepoPath repo </> newName
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
darcsDelete :: DarcsFileStore -> ResourceName -> Author -> String -> IO ()
darcsDelete repo name author logMsg = do
  runShellCommand (darcsRepoPath repo) Nothing "rm" [name]
  darcsCommit repo [name] author logMsg

-- | Commit changes to a resource.  Raise 'Unchanged' exception if there were
-- no changes.
darcsCommit :: DarcsFileStore -> [ResourceName] -> Author -> String -> IO ()
darcsCommit repo names author logMsg = do
  let args = ["--all", "-A", (authorName author ++ " <" ++ authorEmail author ++ ">"), "-m", logMsg] ++ names
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
  -- Just in case it hasn't been added yet; we ignore failures since darcs will
  -- fail if the file doesn't exist *and* if the file exists but has been added already.
  runDarcsCommand repo "add" [name]
  darcsCommit repo [name] author logMsg
  
darcsIndex :: DarcsFileStore ->IO [ResourceName]
darcsIndex repo = do
    (status, errOutput, output) <- runDarcsCommand repo "query"  ["manifest"]
    if status == ExitSuccess
    -- We need to do drop 2 because Darcs returns files like ["./foobar"], and 
    -- the ./ is unexpected.
     then return (map (drop 2) . lines . toString $ output)
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

