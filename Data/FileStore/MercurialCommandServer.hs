{-# LANGUAGE DeriveDataTypeable #-}
{- |
   Module      : Data.FileStore.MercurialCommandServer
   Copyright   : Copyright (C) 2011 John Lenz (lenz@math.uic.edu)
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : GHC 6.10 required

   In version 1.9, mercurial introduced a command server which allows
   a single instance of mercurial to be launched and multiple commands
   can be executed without requiring mercurial to start and stop.  See
   http://mercurial.selenic.com/wiki/CommandServer
-}

module Data.FileStore.MercurialCommandServer
    ( runMercurialCommand
    , rawRunMercurialCommand
    )
where

import Control.Applicative ((<$>))
import Control.Exception (Exception, onException, throwIO)
import Control.Monad (when)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Char (isLower, isUpper)
import Data.FileStore.Utils (runShellCommand)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)
import Data.List (intercalate, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose, hPutStr, hFlush)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (runInteractiveProcess)

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.Map as M
import qualified System.Info as SI

-- | Maximum number of servers to keep around
maxPoolSize :: Int
maxPoolSize = 2

-- | Run a mercurial command and return error status, error output, standard output.  The repository
-- is used as working directory.
runMercurialCommand :: FilePath -> String -> [String] -> IO (ExitCode, String, BL.ByteString)
runMercurialCommand repo command args = do
  server <- getServer repo
  case server of
     Nothing -> rawRunMercurialCommand repo command args
     Just h  -> do ret <- runMercurialServer command args h `onException` cleanupServer h
                   putServer repo h
                   return ret

-- | Run a mercurial command directly without using the server.
rawRunMercurialCommand :: FilePath -> String -> [String] -> IO (ExitCode, String, BL.ByteString)
rawRunMercurialCommand repo command args = do
   let env = [("HGENCODING","utf8")]
   (status, err, out) <- runShellCommand repo (Just env) "hg" (command : args)
   return (status, LUTF8.toString err, out)

-- | Create a new command server for the given repository
createServer :: FilePath -> IO (Handle,Handle,Handle)
createServer repo = do
    (hin,hout,herr,_) <- runInteractiveProcess "hg" ["serve", "--cmdserver", "pipe"] (Just repo) Nothing
    hello <- readMessage hout
    case hello of
       MessageO _ -> return (hin,hout,herr)
       MessageE x -> throwIO $ MercurialServerException (UTF8.toString x)
       _          -> throwIO $ MercurialServerException "unknown hello message"

-- | Cleanup a command sever.  Mercurial will automatically exit itself
--   when the handles are closed.
cleanupServer :: (Handle,Handle,Handle) -> IO ()
cleanupServer (hin,hout,herr) = hClose hin >> hClose hout >> hClose herr

-- | format a command for sending to the server
formatCommand :: String -> [String] -> B.ByteString
formatCommand cmd args = UTF8.fromString $ intercalate "\0" $ cmd : args

-- | run a command using the mercurial server
runMercurialServer :: String -> [String] -> (Handle,Handle,Handle) -> IO (ExitCode, String, BL.ByteString)
runMercurialServer cmd args (hin,hout,herr) = do
    hPutStr hin "runcommand\n"
    let fcmd = formatCommand cmd args
    hWriteWord32be hin $ fromIntegral $ B.length fcmd
    B.hPut hin fcmd
    hFlush hin
    processUntilR hout herr

-- | Read messages from the server until the command finishes or an error message appears
processUntilR :: Handle -> Handle -> IO (ExitCode, String, BL.ByteString)
processUntilR hout _ = loop BL.empty BL.empty
  where loop out err =
          do m <- readMessage hout
             case m of
                MessageO x -> loop (BL.append out $ BL.fromChunks [x]) err
                MessageE x -> loop out (BL.append err $ BL.fromChunks [x])
                MessageR c -> if c == 0
                                then return (ExitSuccess, "", out)
                                else return (ExitFailure c, LUTF8.toString err, out)

data MercurialMessage = MessageO B.ByteString
                      | MessageE B.ByteString
                      | MessageR Int

data MercurialServerException = MercurialServerException String
  deriving (Show,Typeable)
instance Exception MercurialServerException

-- | Read a single message
readMessage :: Handle -> IO MercurialMessage
readMessage hout = do
    buf <- B.hGet hout 1
    when (buf == B.empty) $
       throwIO $ MercurialServerException "Unknown channel"
    let c = B8.head buf
    -- Mercurial says unknown lower case channels can be ignored, but upper case channels
    -- must be handled.  Currently there are two upper case channels, 'I' and 'L' which
    -- are both used for user input/output.  So error on any upper case channel.
    when (isUpper c) $
       throwIO $ MercurialServerException $ "Unknown channel " ++ show c
    len <- hReadWord32be hout
    bdata <- B.hGet hout len
    when (B.length bdata /= len) $
       throwIO $ MercurialServerException "Mercurial did not produce enough output"
    case c of
      'r' | len >= 4 -> return $ MessageR $ bsReadWord32be bdata
      'r'            -> throwIO $ MercurialServerException $ "return value is fewer than 4 bytes"
      'o'            -> return $ MessageO bdata
      'e'            -> return $ MessageE bdata
      _ | isLower c  -> readMessage hout -- skip this message
      _              -> throwIO $ MercurialServerException $ "Unknown channel " ++ show c

-- | Read a 32-bit big-endian into an Int
hReadWord32be :: Handle -> IO Int
hReadWord32be h = do
    s <- B.hGet h 4
    when (B.length s /= 4) $
    	throwIO $ MercurialServerException "unable to read int"
    return $ bsReadWord32be s

-- | Read a 32-bit big-endian from a bytestring into an Int
bsReadWord32be :: B.ByteString -> Int
bsReadWord32be s = (fromIntegral (s `B.index` 0) `shiftL` 24) .|.
                   (fromIntegral (s `B.index` 1) `shiftL` 16) .|.
                   (fromIntegral (s `B.index` 2) `shiftL`  8) .|.
                   (fromIntegral (s `B.index` 3) )

-- | Write a Word32 in big-endian to the handle
hWriteWord32be :: Handle -> Word32 -> IO ()
hWriteWord32be h w = B.hPut h buf
  where buf = B.pack [  -- fromIntegeral to convert to Word8
                fromIntegral (w `shiftR` 24),
                fromIntegral (w `shiftR` 16),
                fromIntegral (w `shiftR`  8),
                fromIntegral w
              ]

-------------------------------------------------------------------
-- Maintain a pool of mercurial servers.  Currently stored in a
-- global IORef.  The code must provide two functions, to get
-- and put a server from the pool.  The code above takes care of
-- cleaning up if an exception occurs.
-------------------------------------------------------------------

data MercurialGlobalState = MercurialGlobalState {
    useCommandServer :: Maybe Bool
  , serverHandles    :: M.Map FilePath [(Handle,Handle,Handle)]
} deriving (Show)

-- | See http://www.haskell.org/haskellwiki/Top_level_mutable_state
mercurialGlobalVar :: IORef MercurialGlobalState
{-# NOINLINE mercurialGlobalVar #-}
mercurialGlobalVar = unsafePerformIO (newIORef (MercurialGlobalState Nothing M.empty))

-- | Pull a server out of the pool.  Returns nothing if the mercurial version
--   does not support servers.
getServer :: FilePath -> IO (Maybe (Handle, Handle, Handle))
getServer repo = do
    use <- useCommandServer <$> readIORef mercurialGlobalVar
    case use of
      Just False -> return Nothing
      Nothing    -> do isok <- checkVersion
                       atomicModifyIORef mercurialGlobalVar $ \state ->
                          (state { useCommandServer = Just isok }, ())
                       getServer repo
      Just True  -> allocateServer repo

-- | Helper function called once we know that mercurial supports servers
allocateServer :: FilePath -> IO (Maybe (Handle, Handle, Handle))
allocateServer repo = do
    ret <- atomicModifyIORef mercurialGlobalVar $ \state ->
             case M.lookup repo (serverHandles state) of
                Just (x:xs) -> (state { serverHandles = M.insert repo xs (serverHandles state)}, Right x)
                _           -> (state, Left ())
    case ret of
      Right x -> return $ Just x
      Left () -> Just <$> createServer repo

-- | Puts a server back in the pool if the pool is not full,
--   otherwise closes the server.
putServer :: FilePath -> (Handle,Handle,Handle) -> IO ()
putServer repo h = do
    ret <- atomicModifyIORef mercurialGlobalVar $ \state -> do
              case M.lookup repo (serverHandles state) of
                  Just xs | length xs >= maxPoolSize -> (state, Right ())
                  Just xs -> (state { serverHandles = M.insert repo (h:xs) (serverHandles state)}, Left ())
                  Nothing -> (state { serverHandles = M.insert repo [h] (serverHandles state)}, Left ())
    case ret of
      Right () -> cleanupServer h
      Left  () -> return ()

-- | Check if the mercurial version supports servers
--   On windows, don't even try because talking to hg over a pipe does not
--   currently work correctly.
checkVersion :: IO Bool
checkVersion
    | isOperatingSystem "mingw32" = return False
    | otherwise                   = do
        (status,_,out) <- runShellCommand "." Nothing "hg" ["version", "-q"]
        case status of
          ExitFailure _ -> return False
          ExitSuccess   -> return $ parseVersion (LUTF8.toString out) >= [2,0]

-- | Helps to find out what operating system we are on
--   Example usage:
--      isOperatingSystem "mingw32" (on windows)
--      isOperatingSystem "darwin"
--      isOperatingSystem "linux"
isOperatingSystem :: String -> Bool
isOperatingSystem sys = SI.os == sys

-- | hg version -q returns something like "Mercurial Distributed SCM (version 1.9.1)"
--   This function returns the list [1,9,1]
parseVersion :: String -> [Int]
parseVersion b = if starts then verLst else [0]
  where msg = "Mercurial Distributed SCM (version "
        starts = isPrefixOf msg b
        ver    = takeWhile (/= ')') $ drop (length msg) b
        verLst = map read $ splitOn "." ver
