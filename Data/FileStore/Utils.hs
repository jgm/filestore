{- |
   Module      : Data.FileStore.Utils
   Copyright   : Copyright (C) 2008 John MacFarlane
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
        , isInsideRepo
        , escapeRegexSpecialChars
        , parseMatchLine ) where


import Codec.Binary.UTF8.String (encodeString)
import Control.Monad (liftM)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (isPrefixOf)
import Data.List.Split (splitWhen)
import Data.Maybe (isJust)
import System.Directory (canonicalizePath)
import System.Directory (getTemporaryDirectory, removeFile, findExecutable)
import System.Exit (ExitCode(..))
import System.IO (openTempFile, hClose)
import System.Process (runProcess, waitForProcess)
import qualified Data.ByteString.Lazy as B

import Data.FileStore.Types (SearchMatch(..))

-- | Run shell command and return error status, standard output, and error output.  Assumes
-- UTF-8 locale. Note that this does not actuall go through \/bin\/sh!
runShellCommand :: FilePath                     -- ^ Working directory
                -> Maybe [(String, String)]     -- ^ Environment
                -> String                       -- ^ Command
                -> [String]                     -- ^ Arguments
                -> IO (ExitCode, B.ByteString, B.ByteString)
runShellCommand workingDir environment command optionList = do
  tempPath <- catch getTemporaryDirectory (\_ -> return ".")
  (outputPath, hOut) <- openTempFile tempPath "out"
  (errorPath, hErr) <- openTempFile tempPath "err"
  hProcess <- runProcess (encodeString command) (map encodeString optionList) (Just workingDir) environment Nothing (Just hOut) (Just hErr)
  status <- waitForProcess hProcess
  errorOutput <- B.readFile errorPath
  output <- B.readFile outputPath
  removeFile errorPath
  removeFile outputPath
  return (status, errorOutput, output)

-- | Do a three way merge, using either git merge-file or RCS merge.  Assumes
-- that either @git@ or @merge@ is in the system path.  Assumes UTF-8 locale.
mergeContents :: (String, B.ByteString)     -- ^ (label, contents) of edited version
              -> (String, B.ByteString)     -- ^ (label, contents) of original revision
              -> (String, B.ByteString)     -- ^ (label, contents) of latest version
              -> IO (Bool, String)          -- ^ (were there conflicts?, merged contents)
mergeContents (newLabel, newContents) (originalLabel, originalContents) (latestLabel, latestContents) = do
  tempPath <- catch getTemporaryDirectory (\_ -> return ".")
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

-- | Inquire of a certain repository whether another file lies within its ambit.
--   This is basically asking whether the file is 'above' the repository in the filesystems's
--   directory tree. Useful for checking the legality of a filename.
isInsideRepo :: FilePath -> FilePath -> IO Bool
isInsideRepo repo name = do
  gitRepoPathCanon <- canonicalizePath repo
  filenameCanon <- canonicalizePath name
  return (gitRepoPathCanon `isPrefixOf` filenameCanon)

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
