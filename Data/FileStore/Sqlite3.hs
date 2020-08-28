{- |
   Module      : Data.FileStore.Sqlite3
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : GHC 6.10 required

   A versioned filestore implemented using sqlite3.
   Normally this module should not be imported: import
   "Data.FileStore" instead.
-}

module Data.FileStore.Sqlite3
           ( sqlite3FileStore
           )
where
import Data.FileStore.Types
import System.Exit
import System.IO.Error (isDoesNotExistError)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.FileStore.Utils (hashsMatch, isInsideRepo, runShellCommand) 
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B
import qualified Text.ParserCombinators.Parsec as P
import Data.Char (chr)
import Control.Monad (liftM, unless, when)
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, executable, getPermissions, setPermissions)
import Control.Exception (throwIO)
import Database.HDBC

-- | Return a filestore implemented using the sqlite3 distributed revision control system
-- (<http://sqlite3-scm.com/>).
sqlite3FileStore :: IConnection c => c -> FileStore
sqlite3FileStore repo = FileStore {
    initialize        = sqlite3Init repo
  , save              = sqlite3Save repo 
  , retrieve          = sqlite3Retrieve repo
  , delete            = sqlite3Delete repo
  , rename            = sqlite3Move repo
  , history           = sqlite3Log repo
  , latest            = sqlite3LatestRevId repo
  , revision          = sqlite3GetRevision repo
  , index             = sqlite3Index repo
  , search            = sqlite3Search repo 
  , idsMatch          = const hashsMatch repo
  }

-- | Initialize a repository, creating the directory if needed.
sqlite3Init :: IConnection c => c -> IO ()
sqlite3Init conn =
  catchSql createTables $ \_ -> throwIO RepositoryExists
    where createTables = do
           run conn "CREATE TABLE resources(id INTEGER PRIMARY KEY, name TEXT, latestChangeId INTEGER);" []
           run conn "CREATE TABLE revisions( id INTEGER PRIMARY KEY, author TEXT, email TEXT, description TEXT, datetime DATETIME);" []
           run conn "CREATE TABLE changes( id INTEGER PRIMARY KEY, revisionId INTEGER, changetype INTEGER, resourceId INTEGER, contents BLOB);" []
           commit conn

-- | Save changes (creating file and directory if needed), add, and commit.
sqlite3Save :: (Contents a, IConnection c) => c -> ResourceName -> Author -> String -> a -> IO ()
sqlite3Save conn name author logMsg contents = undefined
  -- create a revision entry
  -- create a change entry
  -- update resources

-- | Retrieve contents from resource.
sqlite3Retrieve :: (Contents a, IConnection c)
                => c
                -> ResourceName
                -> Maybe RevisionId    -- ^ @Just@ revision ID, or @Nothing@ for latest
                -> IO a
sqlite3Retrieve conn name Nothing = undefined
sqlite3Retrieve conn name (Just revid) = undefined

-- | Delete a resource from the database.
sqlite3Delete :: IConnection c => c -> ResourceName -> Author -> String -> IO ()
sqlite3Delete conn name author logMsg = undefined

-- | Change the name of a resource.
sqlite3Move :: IConnection c => c -> ResourceName -> ResourceName -> Author -> String -> IO ()
sqlite3Move conn oldName newName author logMsg = undefined

-- | Return revision ID for latest commit for a resource.
sqlite3LatestRevId :: IConnection c => c -> ResourceName -> IO RevisionId
sqlite3LatestRevId conn name = undefined

-- | Get revision information for a particular revision ID, or latest revision.
sqlite3GetRevision :: IConnection c => c -> RevisionId -> IO Revision
sqlite3GetRevision conn revid = undefined

-- | Get list of files in database.
sqlite3Index :: IConnection c => c -> IO [ResourceName]
sqlite3Index conn = undefined

-- | Uses sqlite3 fts to search database.
sqlite3Search :: IConnection c => c -> SearchQuery -> IO [SearchMatch]
sqlite3Search conn query = undefined

-- | Return list of log entries for the given time frame and list of resources.
-- If list of resources is empty, log entries for all resources are returned.
sqlite3Log :: IConnection c => c -> [ResourceName] -> TimeRange -> IO [Revision]
sqlite3Log conn names (TimeRange mbSince mbUntil) = undefined

