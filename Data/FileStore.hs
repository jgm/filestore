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
import Control.Exception (throwIO)

class FileStore b where

    -- A default is provided for modify, so an implementation need not
    -- define this.  A default is also provided for diff, though a
    -- custom implementation may be more efficient.  idsMatch is defined
    -- as equality by default, but may be redefined if needed.  For example,
    -- git, IDs can be abbreviated, and two IDs match if one is a prefix of the other.
    
    initialize     :: b -> IO ()
    save           :: Contents a => b -> ResourceName -> Author -> String -> a -> IO ()
    create         :: Contents a => b -> ResourceName -> Author -> String -> a -> IO ()
    modify         :: Contents a => b -> ResourceName -> RevisionId -> Author -> String -> a -> IO ()
    retrieve       :: Contents a => b -> ResourceName -> Maybe RevisionId -> IO a
    delete         :: b -> ResourceName -> Author -> String -> IO ()
    move           :: b -> ResourceName -> Author -> String -> IO ()
    history        :: b -> [ResourceName] -> TimeRange -> IO History
    revision       :: b -> ResourceName -> Maybe RevisionId -> IO Revision
    index          :: b -> IO [ResourceName]
    diff           :: b -> ResourceName -> RevisionId -> RevisionId -> IO String
    idsMatch       :: b -> RevisionId -> RevisionId -> Bool

    modify           =  modifyWithMerge
    diff             =  genericDiff
    idsMatch  _      =  (==) 

modifyWithMerge :: (Contents a, FileStore b) => b -> ResourceName -> RevisionId -> Author -> String -> a -> IO ()
modifyWithMerge fs name originalRevId author msg contents = do
  latestRev <- revision fs name Nothing
  let latestRevId = revId latestRev
  if idsMatch fs originalRevId latestRevId
     then save fs name author msg contents
     else do
       latestContents <- retrieve fs name (Just latestRevId)
       originalContents <- retrieve fs name (Just originalRevId)
       (conflicts, mergedText) <- mergeContents ("edited", toByteString contents) (originalRevId, originalContents) (latestRevId, latestContents)
       throwIO $ Merged (MergeInfo latestRev conflicts mergedText)

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
    create     = gitCreate
    retrieve   = gitRetrieve
    delete     = gitDelete
    move       = gitMove
    history    = gitHistory
    revision   = gitGetRevision
    index      = gitIndex
    diff       = gitDiff
    idsMatch   = gitIdsMatch

instance SearchableFileStore GitFileStore where 
    search     = gitSearch

