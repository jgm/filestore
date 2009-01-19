{-# LANGUAGE Rank2Types, TypeSynonymInstances, DeriveDataTypeable #-}
{- |
   Module      : Data.FileStore.Types
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : GHC 6.10 required

   Type definitions for "Data.FileStore".
-}

module Data.FileStore.Types
           ( RevisionId
           , ResourceName
           , Author(..)
           , Change(..)
           , Description
           , Revision(..)
           , Contents(..)
           , TimeRange(..)
           , MergeInfo(..)
           , FileStoreError(..)
           , SearchMatch(..)
           , SearchQuery(..)
           , defaultSearchQuery
           , DateTime
           , FileStore (..) )

where
import Data.ByteString.Lazy (ByteString)
import Data.Typeable
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Data.DateTime (DateTime)
import Control.Exception (Exception)
import Prelude hiding (catch)

type RevisionId   = String

type ResourceName = String

data Author =
  Author {
    authorName  :: String
  , authorEmail :: String
  } deriving (Show, Read, Eq, Typeable)

data Change =
    Added ResourceName
  | Deleted ResourceName
  | Modified ResourceName
  deriving (Show, Read, Eq, Typeable)

type Description = String

data Revision =
  Revision {
    revId          :: RevisionId
  , revDateTime    :: DateTime
  , revAuthor      :: Author
  , revDescription :: Description
  , revChanges     :: [Change]
  } deriving (Show, Read, Eq, Typeable)

class Contents a where
  fromByteString :: ByteString -> a
  toByteString   :: a -> ByteString

instance Contents ByteString where
  toByteString = id
  fromByteString = id

instance Contents String where
  toByteString   = fromString
  fromByteString = toString

data TimeRange =
  TimeRange {
    timeFrom :: Maybe DateTime  -- ^ @Nothing@ means no lower bound
  , timeTo   :: Maybe DateTime  -- ^ @Nothing@ means no upper bound
  } deriving (Show, Read, Eq, Typeable)

data MergeInfo =
  MergeInfo {
    mergeRevision  :: Revision   -- ^ The revision with which changes were merged
  , mergeConflicts :: Bool       -- ^ @True@ if there were merge conflicts
  , mergeText      :: String     -- ^ The merged text, including conflict markers
  } deriving (Show, Read, Eq, Typeable)

data FileStoreError =
    RepositoryExists             -- ^ Tried to initialize a repository that already exists
  | ResourceExists               -- ^ Tried to create a resource that already exists
  | NotFound                     -- ^ Requested resource was not found
  | IllegalResourceName          -- ^ The specified resource name is illegal
  | Unchanged                    -- ^ The resource was not modified, because the contents were unchanged
  | UnsupportedOperation
  | UnknownError String
  deriving (Read, Eq, Typeable)

instance Show FileStoreError where
  show RepositoryExists      = "RepositoryExists"
  show ResourceExists        = "ResourceExists"
  show NotFound              = "NotFound"
  show IllegalResourceName   = "IllegalResourceName"
  show Unchanged             = "Unchanged"
  show UnsupportedOperation  = "UnsupportedOperation"
  show (UnknownError s)      = "UnknownError: " ++ s

instance Exception FileStoreError

data SearchQuery =
  SearchQuery {
    queryPatterns    :: [String] -- ^ Patterns to match
  , queryWholeWords  :: Bool     -- ^ Match patterns only with whole words?
  , queryMatchAll    :: Bool     -- ^ Return matches only from files in which all patterns match?
  , queryIgnoreCase  :: Bool     -- ^ Make matches case-insensitive?
  } deriving (Show, Read, Eq, Typeable)

defaultSearchQuery :: SearchQuery
defaultSearchQuery = SearchQuery {
     queryPatterns   = []
   , queryWholeWords = True
   , queryMatchAll   = True
   , queryIgnoreCase = True
   }

data SearchMatch =
  SearchMatch {
    matchResourceName :: ResourceName
  , matchLineNumber   :: Integer
  , matchLine         :: String
  } deriving (Show, Read, Eq, Typeable)

-- | A versioning filestore, which can be implemented using the
-- file system, a database, or revision-control software.
data FileStore = FileStore {

    -- | Initialize a new filestore.
    initialize     :: IO ()

    -- | Save contents in the filestore.
  , save           :: Contents a
                   => ResourceName      -- ^ Resource to save.
                   -> Author            -- ^ Author of change.
                   -> String            -- ^ Description of change.
                   -> a                 -- ^ New contents of resource.
                   -> IO ()
    
    -- | Retrieve the contents of the named resource.
  , retrieve       :: Contents a
                   => ResourceName      -- ^ Resource to retrieve.
                   -> Maybe RevisionId  -- ^ @Just@ a particular revision ID, or @Nothing@ for latest
                   -> IO a

    -- | Delete a named resource, providing author and log message.
  , delete         :: ResourceName      -- ^ Resource to delete.
                   -> Author            -- ^ Author of change.
                   -> String            -- ^ Description of change.
                   -> IO ()

    -- | Rename a resource, providing author and log message.
  , rename         :: ResourceName      -- ^ Resource original name.
                   -> ResourceName      -- ^ Resource new name.
                   -> Author            -- ^ Author of change.
                   -> String            -- ^ Description of change.
                   -> IO ()

    -- | Get history for a list of named resources in a (possibly openended) time range.
    -- If the list is empty, history for all resources will be returned. 
  , history        :: [ResourceName]    -- ^ List of resources to get history for, or @[]@ for all.
                   -> TimeRange         -- ^ Time range within which to get history.
                   -> IO [Revision]

    -- | Return the revision ID of the latest change for a resource.  Raises 'NotFound'
    -- if the resource is not found.
  , latest         :: ResourceName      -- ^ Resource to get revision ID for.
                   -> IO RevisionId

    -- | Return information about a revision, given the ID.  Raises 'NotFound' if there is
    -- no such revision.
  , revision       :: RevisionId        -- ^ Revision ID to get revision information for.
                   -> IO Revision

    -- | Return a list of resources in the filestore.
  , index          :: IO [ResourceName]

    -- | @True@ if the revision IDs match, in the sense that the
    -- can be treated as specifying the same revision.
  , idsMatch       :: RevisionId
                   -> RevisionId
                   -> Bool

  -- | Search the filestore for patterns. 
  , search         :: SearchQuery
                   -> IO [SearchMatch]

  }

