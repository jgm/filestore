{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{- Abstract interface to a versioned file store, which could be
-  implemented using a VCS or a database.

   (C) 2008 John MacFarlane
   Based on ideas from Network.Orchid.Core.Backend.
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

class FileStore b where

    -- A minimal implementation will define the following functions:

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
    history        :: b -> [ResourceName] -> TimeRange -> IO History

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

    -- Sensible defaults are provided for modify, create, and diff, so
    -- implementations will generally not need to implement these.

    modify           = modifyWithMerge
    create           = genericCreate 
    diff             = genericDiff

    -- idsMatch is defined as equality by default, but may be redefined
    -- if needed.  For example, in git, IDs can be abbreviated, and two
    -- IDs match if one is a prefix of the other.

    idsMatch  _      = (==) 


handleUnknownError :: SomeException -> IO a
handleUnknownError e = throwIO $ UnknownError $ show e

genericCreate :: (Contents a, FileStore b) => b -> ResourceName -> Author -> String -> a -> IO ()
genericCreate fs name author logMsg contents = do
  catch (revision fs name Nothing >> throwIO ResourceExists)
        (\e -> if e == NotFound
                  then save fs name author logMsg contents
                  else throwIO e)

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

genericDiff :: FileStore b => b -> ResourceName -> RevisionId -> RevisionId -> IO String
genericDiff fs name id1 id2 = do
  contents1 <- retrieve fs name (Just id1)
  contents2 <- retrieve fs name (Just id2)
  diffContents contents1 contents2

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

