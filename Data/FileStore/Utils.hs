{- Auxiliary functions for running shell commands.

   Note:  UTF-8 locale is assumed.
-}

module Data.FileStore.Utils where

import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode)
import System.IO (openTempFile)
import Prelude hiding (readFile)
import qualified Data.ByteString.Lazy as B
import System.Process (runProcess, waitForProcess)
import Codec.Binary.UTF8.String (encodeString)

-- | Run shell command and return error status, standard output, and error output.
runShellCommand :: FilePath -> Maybe [(String, String)] -> String -> [String] -> IO (ExitCode, B.ByteString, B.ByteString)
runShellCommand workingDir environment command optionList = do
  tempPath <- getTemporaryDirectory
  (outputPath, hOut) <- openTempFile tempPath "out"
  (errorPath, hErr) <- openTempFile tempPath "err"
  hProcess <- runProcess command optionList (Just workingDir) environment Nothing (Just hOut) (Just hErr)
  status <- waitForProcess hProcess
  errorOutput <- B.readFile errorPath
  output <- B.readFile outputPath
  removeFile errorPath
  removeFile outputPath
  return (status, errorOutput, output)

runProgCommand :: String -> Maybe [(String, String)] -> String -> String -> [String] -> IO (ExitCode, B.ByteString, B.ByteString)
runProgCommand workingDir environment prog command args =
  runShellCommand workingDir environment prog (command : map encodeString args)
