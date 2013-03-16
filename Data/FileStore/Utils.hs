{-# LANGUAGE CPP, ScopedTypeVariables #-}
{- |
   Module      : Data.FileStore.Utils
   Copyright   : Copyright (C) 2009 John MacFarlane, Gwern Branwen
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

   Utility functions for running external processes.
-}

module Data.FileStore.Utils (
          runShellCommand
        , mergeContents
        , hashsMatch
        , escapeRegexSpecialChars
        , parseMatchLine
        , splitEmailAuthor
        , ensureFileExists
        , regSearchFiles
        , regsSearchFile
        , withSanityCheck
        , grepSearchRepo 
        , withVerifyDir
        , encodeArg ) where

import Control.Exception (throwIO)
import Control.Applicative ((<$>))
import Control.Monad (liftM, liftM2, when, unless)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Char (isSpace)
import Data.List (intersect, nub, isPrefixOf, isInfixOf)
import Data.List.Split (splitWhen)
import Data.Maybe (isJust)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile, findExecutable, createDirectoryIfMissing, getDirectoryContents)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO (openTempFile, hClose)
import System.IO.Error (isDoesNotExistError)
import System.Process (runProcess, waitForProcess)
import System.Environment (getEnvironment)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import qualified Control.Exception as E
#if MIN_VERSION_base(4,5,0)
#else
import Codec.Binary.UTF8.String (encodeString)
#endif

import Data.FileStore.Types (SearchMatch(..), FileStoreError(IllegalResourceName, NotFound, UnknownError), SearchQuery(..))

-- | Encode argument for raw command.
encodeArg :: String -> String
#if MIN_VERSION_base(4,5,0)
encodeArg = id
#else
encodeArg = encodeString
#endif

-- | Run shell command and return error status, standard output, and error output.  Assumes
-- UTF-8 locale. Note that this does not actually go through \/bin\/sh!
runShellCommand :: FilePath                     -- ^ Working directory
                -> Maybe [(String, String)]     -- ^ Environment
                -> String                       -- ^ Command
                -> [String]                     -- ^ Arguments
                -> IO (ExitCode, B.ByteString, B.ByteString)
runShellCommand workingDir environment command optionList = do
  tempPath <- E.catch getTemporaryDirectory (\(_ :: E.SomeException) -> return ".")
  (outputPath, hOut) <- openTempFile tempPath "out"
  (errorPath, hErr) <- openTempFile tempPath "err"
  env <- liftM2 (++) environment . Just <$> getEnvironment
  hProcess <- runProcess (encodeArg command) (map encodeArg optionList) (Just workingDir) env Nothing (Just hOut) (Just hErr)
  status <- waitForProcess hProcess
  errorOutput <- S.readFile errorPath
  output <- S.readFile outputPath
  removeFile errorPath
  removeFile outputPath
  return (status, B.fromChunks [errorOutput], B.fromChunks [output])

-- | Do a three way merge, using either git merge-file or RCS merge.  Assumes
-- that either @git@ or @merge@ is in the system path.  Assumes UTF-8 locale.
mergeContents :: (String, B.ByteString)     -- ^ (label, contents) of edited version
              -> (String, B.ByteString)     -- ^ (label, contents) of original revision
              -> (String, B.ByteString)     -- ^ (label, contents) of latest version
              -> IO (Bool, String)          -- ^ (were there conflicts?, merged contents)
mergeContents (newLabel, newContents) (originalLabel, originalContents) (latestLabel, latestContents) = do
  tempPath <- E.catch getTemporaryDirectory (\(_ :: E.SomeException) -> return ".")
  (originalPath, hOriginal) <- openTempFile tempPath "orig"
  (latestPath, hLatest)     <- openTempFile tempPath "latest"
  (newPath, hNew)           <- openTempFile tempPath "new"
  B.hPutStr hOriginal originalContents >> hClose hOriginal
  B.hPutStr hLatest latestContents >> hClose hLatest
  B.hPutStr hNew newContents >> hClose hNew
  gitExists <- liftM isJust (findExecutable "git")
  (conflicts, mergedContents) <-
    if gitExists
       then do
         (status, err, out) <- runShellCommand tempPath Nothing "git" ["merge-file", "--stdout", "-L", newLabel, "-L",
                                     originalLabel, "-L", latestLabel, newPath, originalPath, latestPath]
         case status of
              ExitSuccess             -> return (False, out)
              ExitFailure n | n >= 0  -> return (True, out)
              _                       -> error $ "merge failed: " ++ toString err
       else do
         mergeExists <- liftM isJust (findExecutable "merge")
         if mergeExists
            then do
               (status, err, out) <- runShellCommand tempPath Nothing "merge" ["-p", "-q", "-L", newLabel, "-L",
                                          originalLabel, "-L", latestLabel, newPath, originalPath, latestPath]
               case status of
                    ExitSuccess             -> return (False, out)
                    ExitFailure 1           -> return (True, out)
                    _                       -> error $ "merge failed: " ++ toString err
            else error "mergeContents requires 'git' or 'merge', and neither was found in the path."
  removeFile originalPath
  removeFile latestPath
  removeFile newPath
  return (conflicts, toString mergedContents)

escapeRegexSpecialChars :: String -> String
escapeRegexSpecialChars = backslashEscape "?*+{}[]\\^$.()"
  where backslashEscape chars (x:xs) | x `elem` chars = '\\' : x : backslashEscape chars xs
        backslashEscape chars (x:xs)                  = x : backslashEscape chars xs
        backslashEscape _ []                          = []

-- | A number of VCS systems uniquely identify a particular revision or change via a
--   cryptographic hash of some sort. These hashs can be very long, and so systems like
--   Git and Darcs don't require the entire hash - a *unique prefix*. Thus a definition
--   of hash equality is '==', certainly, but also simply whether either is a prefix of the
--   other. If both are reasonably long, then the likelihood the shorter one is not a unique
--   prefix of the longer (that is, clashes with another hash) is small.
--   The burden of proof is on the caller to not pass a uselessly short short-hash like '1', however.
hashsMatch :: (Eq a) => [a] -> [a] -> Bool
hashsMatch r1 r2 = r1 `isPrefixOf` r2 || r2 `isPrefixOf` r1

-- | Inquire of a certain directory whether another file lies within its ambit.
--   This is basically asking whether the file is 'above' the directory in the filesystems's
--   directory tree. Useful for checking the legality of a filename.
--   Note: due to changes in canonicalizePath in ghc 7, we no longer have
--   a reliable way to do this; so isInsideDir is False whenever either
--   the file or the directory contains "..".
isInsideDir :: FilePath -> FilePath -> Bool
isInsideDir name dir = dir `isPrefixOf` name
  && not (".." `isInfixOf` dir) && not (".." `isInfixOf` name)

-- | A parser function. This is intended for use on strings which are output by grep programs
--   or programs which mimic the standard grep output - which uses colons as delimiters and has
--   3 fields: the filename, the line number, and then the matching line itself. Note that this 
--   is for use on only strings meeting that format - if it goes "file:match", this will throw
--   a pattern-match exception.
--
-- > parseMatchLine "foo:10:bar baz quux" ~> 
-- > SearchMatch {matchResourceName = "foo", matchLineNumber = 10, matchLine = "bar baz quux"}
parseMatchLine :: String -> SearchMatch
parseMatchLine str =
  let (fn:n:res:_) = splitWhen (==':') str
  in  SearchMatch{matchResourceName = fn, matchLineNumber = read n, matchLine = res}

-- | Our policy is: if the input is clearly a "name \<e\@mail.com\>" input, then we return "(Just Address, Name)"
--   If there is no '<' in the input, then it clearly can't be of that format, and so we just return "(Nothing, Name)"
--
-- > splitEmailAuthor "foo bar baz@gmail.com" ~> (Nothing,"foo bar baz@gmail.com")
-- > splitEmailAuthor "foo bar <baz@gmail.com>" ~> (Just "baz@gmail.com","foo bar")
splitEmailAuthor :: String -> (Maybe String, String)
splitEmailAuthor x = (mbEmail, trim name)
  where (name, rest) = break (=='<') x
        mbEmail = if null rest
                     then Nothing
                     else Just $ takeWhile (/='>') $ drop 1 rest

-- | Trim leading and trailing spaces
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Search multiple files with a single regexp.
--   This calls out to grep, and so supports the regular expressions grep does.
regSearchFiles :: FilePath -> [String] -> String -> IO [String]
regSearchFiles repo filesToCheck pattern = do (_, _, result) <- runShellCommand repo
                                                             Nothing  "grep" $ ["--line-number", "-I", "-l", "-E", "-e", pattern] ++ filesToCheck
                                              let results = intersect filesToCheck $ lines $ toString result
                                              return results

-- | Search a single file with multiple regexps.
regsSearchFile :: [String] -> FilePath -> [String] -> String -> IO [String]
regsSearchFile os repo patterns file = do res <- mapM (run file) patterns
                                          return $ nub $ concat res
      where run f p = do (_,_,r) <- runShellCommand repo Nothing "grep" (os ++ [p, f])
                         return $ lines $ toString r

-- | If name doesn't exist in repo or is not a file, throw 'NotFound' exception.
ensureFileExists :: FilePath -> FilePath -> IO ()
ensureFileExists repo name = do
  isFile <- doesFileExist (repo </> encodeArg name)
  unless isFile $ throwIO NotFound

-- | Check that the filename/location is within the given repo, and not inside
-- any of the (relative) paths in @excludes@.  Create the directory if needed.
-- If everything checks out, then perform the specified action.
withSanityCheck :: FilePath
                -> [FilePath]
                -> FilePath
                -> IO b 
                -> IO b
withSanityCheck repo excludes name action = do
  let filename = repo </> encodeArg name
  let insideRepo = filename `isInsideDir` repo
  let insideExcludes = or $ map (filename `isInsideDir`)
                          $ map (repo </>) excludes
  when (insideExcludes || not insideRepo)
    $ throwIO IllegalResourceName
  createDirectoryIfMissing True $ takeDirectory filename
  action

-- | Uses grep to search a file-based repository. Note that this calls out to grep; and so
--   is generic over repos like git or darcs-based repos. (The git FileStore instance doesn't
--   use this because git has builtin grep functionality.)
--   Expected usage is to specialize this function with a particular backend's 'index'.
grepSearchRepo :: (FilePath -> IO [String]) -> FilePath -> SearchQuery -> IO [SearchMatch]
grepSearchRepo indexer repo query = do
  let opts = ["-I", "--line-number", "--with-filename"] ++
             ["-i" | queryIgnoreCase query] ++
             (if queryWholeWords query then ["--word-regexp"] else ["-E"])
  let regexps = map escapeRegexSpecialChars $ queryPatterns query
  files <- indexer repo
  if queryMatchAll query
     then do
       filesMatchingAllPatterns <- liftM (foldr1 intersect) $ mapM (regSearchFiles repo files) regexps
       output <- mapM (regsSearchFile opts repo regexps) filesMatchingAllPatterns
       return $ map parseMatchLine $ concat output
     else do
       (_status, _errOutput, output) <-
            runShellCommand repo Nothing "grep" $ opts ++
                                                  concatMap (\term -> ["-e", term]) regexps ++
                                                  files
       let results = lines $ toString output
       return $ map parseMatchLine results

-- | we don't actually need the contents, just want to check that the directory exists and we have enough permissions
withVerifyDir :: FilePath -> IO a -> IO a
withVerifyDir d a =
  E.catch (liftM head (getDirectoryContents $ encodeArg d) >> a) $ \(e :: E.IOException) ->
    if isDoesNotExistError e
       then throwIO NotFound
       else throwIO . UnknownError . show $ e
