{- Auxiliary functions for running shell commands.
-}

module Data.FileStore.Utils (
          runShellCommand
        , mergeContents)
where

import System.Directory (getTemporaryDirectory, removeFile, findExecutable)
import System.Exit (ExitCode(..))
import System.IO (openTempFile, hClose)
import qualified Data.ByteString.Lazy as B
import System.Process (runProcess, waitForProcess)
import Codec.Binary.UTF8.String (encodeString)
import Data.ByteString.Lazy.UTF8 (toString)
import Control.Monad (liftM)
import Data.Maybe (isJust)

-- | Run shell command and return error status, standard output, and error output.  Assumes
-- UTF-8 locale.
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

-- | Do a three way merge, using either git merge-file or RCS merge. 
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
