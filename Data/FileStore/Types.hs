{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}
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
           , Revision(..)
           , Contents(..)
           , TimeRange(..)
           , MergeInfo(..)
           , FileStoreError(..)
           , SearchMatch(..)
           , SearchQuery(..)
           , defaultSearchQuery
           , DateTime
           , FileStore (..)
           , SearchableFileStore (..) )

where
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Data.DateTime (DateTime)
import Data.Typeable
import Control.Exception (Exception, throwIO, catch, SomeException)
import Data.FileStore.Utils (mergeContents, diffContents)
import Prelude hiding (catch)

type RevisionId   = String

type ResourceName = String

data Author =
  Author {
    authorName  :: String
  , authorEmail :: String
  } deriving (Show, Read, Eq)

data Revision =
  Revision {
    revId          :: RevisionId
  , revDateTime    :: DateTime
  , revAuthor      :: Author
  , revDescription :: String
  , revModified    :: [ResourceName]
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
    timeFrom :: Maybe DateTime
  , timeTo   :: Maybe DateTime
  } deriving (Show, Read, Eq)

data MergeInfo =
  MergeInfo {
    mergeRevision  :: Revision   -- ^ The revision with which changes were merged
  , mergeConflicts :: Bool       -- ^ @True@ if there were merge conflicts
  , mergeText      :: String     -- ^ The merged text, including conflict markers
  } deriving (Show, Read, Eq, Typeable)

data FileStoreError =
    RepositoryExists
  | ResourceExists
  | NotFound
  | Unchanged
  | UnsupportedOperation
  | UnknownError String
  deriving (Show, Read, Eq, Typeable)

instance Exception FileStoreError

data SearchQuery =
  SearchQuery {
    queryPatterns    :: [String] -- ^ Patterns to match
  , queryRegex       :: Bool     -- ^ Interpret patterns as extended regexes, not strings
  , queryWholeWords  :: Bool     -- ^ Match patterns only with whole words
  , queryMatchAll    :: Bool     -- ^ Return matches only from files in which all patterns match
  , queryIgnoreCase  :: Bool     -- ^ Make matches case-insensitive
  } deriving (Show, Read, Eq)

defaultSearchQuery :: SearchQuery
defaultSearchQuery = SearchQuery {
     queryPatterns   = []
   , queryRegex      = True
   , queryWholeWords = True
   , queryMatchAll   = True
   , queryIgnoreCase = True
   }

data SearchMatch =
  SearchMatch {
    matchResourceName :: ResourceName
  , matchLineNumber   :: Integer
  , matchLine         :: String
  } deriving (Show, Read, Eq)

-- | An abstract class for a versioning filestore, which can be
-- implemented using the file system, a database, or revision-control
-- software. A minimal instance definition will define 'initialize',
-- 'save', 'retrieve', 'delete', 'move', 'history', 'revision', and
-- 'index'. Sensible defaults are provided for 'modify', 'create',
-- 'diff', and 'idsMatch', so these normally do not need to be
-- implemented.
class FileStore b where

    -- | Initialize a new filestore.
    initialize     :: b -> IO ()

    -- | Save contents in the filestore with a resource name, author, and log message.
    save           :: Contents a => b -> ResourceName -> Author -> String -> a -> IO ()
    
    -- | Like save, but first verify that the resource name is new.
    create         :: Contents a => b -> ResourceName -> Author -> String -> a -> IO ()

    -- | Modify a named resource in the filestore.  Like save, except that a revision ID
    -- must be specified.  If the resource has been modified since the specified revision,
    -- merge information is returned.  Otherwise, the new contents are saved.  
    modify         :: Contents a => b -> ResourceName -> RevisionId -> Author -> String -> a -> IO (Either MergeInfo ())

    -- | Retrieve the contents of the named resource.  If @Just@ a revision ID is provided,
    -- that revision will be returned; if @Nothing@, the latest revision will be returned.
    retrieve       :: Contents a => b -> ResourceName -> Maybe RevisionId -> IO a

    -- | Delete a named resource, providing author and log message.
    delete         :: b -> ResourceName -> Author -> String -> IO ()

    -- | Move a named resource, providing author and log message.
    move           :: b -> ResourceName -> ResourceName -> Author -> String -> IO ()

    -- | Get history for a list of named resources in a (possibly openended) time range.
    -- If the list is empty, history for all resources will be returned. 
    history        :: b -> [ResourceName] -> TimeRange -> IO [Revision]

    -- | Return information about a revision, given a resource name and a revision ID,
    -- or the latest revision, if revision ID is @Nothing@.
    revision       :: b -> ResourceName -> Maybe RevisionId -> IO Revision

    -- | Return a list of resources in the filestore.
    index          :: b -> IO [ResourceName]

    -- | Return a unified diff of two revisions of a named resource.
    diff           :: b -> ResourceName -> RevisionId -> RevisionId -> IO String

    -- | @True@ if the revision IDs match, in the sense that the
    -- can be treated as specifying the same revision.
    idsMatch       :: b -> RevisionId -> RevisionId -> Bool

    -- Defaults

    modify           = modifyWithMerge
    create           = genericCreate 
    diff             = genericDiff

    -- There is sometimes reason to redefine idsMatch -- e.g. in git, where revision IDs
    -- can be abbreviated, and two IDs match if one is a prefix of the other.
    idsMatch  _      = (==) 


-- | An abstract class for a searchable versioning filestore.
class FileStore b => SearchableFileStore b where
    search         :: b -> SearchQuery -> IO [SearchMatch]

handleUnknownError :: SomeException -> IO a
handleUnknownError e = throwIO $ UnknownError $ show e

-- | A definition of 'create' in terms of 'revision' and 'save'.  Checks to see
-- if the resource already exists: if so, throws a 'ResourceExists' error, if not, saves contents.
genericCreate :: (Contents a, FileStore b) => b -> ResourceName -> Author -> String -> a -> IO ()
genericCreate fs name author logMsg contents = do
  catch (revision fs name Nothing >> throwIO ResourceExists)
        (\e -> if e == NotFound
                  then save fs name author logMsg contents
                  else throwIO e)

-- | A definition of 'modify' in terms of 'revision', 'retrieve', and 'save'.  Checks to see if
-- the revision that is being modified is the latest revision of the resource.  If it is,
-- the changes are saved and Right () is returned.  If it is not, a three-way merge is
-- performed and Left (information on the merge) is returned.  It is then up to the calling program
-- to call modify again with the updated revision ID and possibly revised contents.
modifyWithMerge :: (Contents a, FileStore b) => b -> ResourceName -> RevisionId -> Author -> String -> a -> IO (Either MergeInfo ())
modifyWithMerge fs name originalRevId author msg contents = do
  latestRev <- revision fs name Nothing
  let latestRevId = revId latestRev
  if idsMatch fs originalRevId latestRevId
     then save fs name author msg contents >> return (Right ())
     else do
       latestContents <- retrieve fs name (Just latestRevId)
       originalContents <- retrieve fs name (Just originalRevId)
       (conflicts, mergedText) <- catch 
                                  (mergeContents ("edited", toByteString contents) (originalRevId, originalContents) (latestRevId, latestContents))
                                  handleUnknownError
       return $ Left (MergeInfo latestRev conflicts mergedText)

-- | A definition of 'diff' in terms of 'retrieve'.  The contents of the two revisions
-- are fetched and run through an external diff program, which produces a unified diff
-- with context lines set to 10,000 (so the whole file will appear, with changes marked
-- by + or - in the left column).
genericDiff :: FileStore b => b -> ResourceName -> RevisionId -> RevisionId -> IO String
genericDiff fs name id1 id2 = do
  contents1 <- retrieve fs name (Just id1)
  contents2 <- retrieve fs name (Just id2)
  diffContents contents1 contents2

