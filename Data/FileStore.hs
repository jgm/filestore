{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{- |
   Module      : Data.FileStore
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : GHC 6.10 required

   Abstract interface to a versioned file store, which can be
   implemented using a revision-control system or database.
   Based on ideas from Sebastiaan Visser's "Network.Orchid.Core.Backend".
-}

module Data.FileStore
           ( FileStore(..)
           , SearchableFileStore(..)
           , module Data.FileStore.Types
           , GitFileStore(..)
           )
where

import Data.FileStore.Git
import Data.FileStore.Types
import Data.FileStore.Utils (mergeContents, diffContents)
import Prelude hiding (catch)
import Control.Exception (throwIO, catch, SomeException)

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

-- | An abstract class for a searchable versioning filestore.
class FileStore b => SearchableFileStore b where
    search         :: b -> SearchQuery -> IO [SearchMatch]

instance FileStore GitFileStore where
    initialize = gitInit
    save       = gitSave
    retrieve   = gitRetrieve
    delete     = gitDelete
    move       = gitMove
    history    = gitHistory
    revision   = gitGetRevision
    index      = gitIndex
    diff       = gitDiff
    idsMatch   = gitIdsMatch
    -- modify, create: defaults

instance SearchableFileStore GitFileStore where 
    search     = gitSearch

