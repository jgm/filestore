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

class FileStore b where
    initialize     :: b -> IO ()
    create         :: Contents a => b -> ResourceName -> Author -> String -> a -> IO ()
    modify         :: Contents a => b -> ResourceName -> RevisionId -> Author -> String -> a -> IO ()
    retrieve       :: Contents a => b -> ResourceName -> Maybe RevisionId -> IO a
    delete         :: b -> ResourceName -> Author -> String -> IO ()
    move           :: b -> ResourceName -> Author -> String -> IO ()
    history        :: b -> [ResourceName] -> TimeRange -> IO History
    revision       :: b -> ResourceName -> Maybe RevisionId -> IO Revision
    index          :: b -> IO [ResourceName]
    diff           :: b -> ResourceName -> RevisionId -> RevisionId -> IO String

class FileStore b => SearchableFileStore b where
    search         :: b -> SearchQuery -> IO [SearchMatch]

instance FileStore GitFileStore where
    initialize = gitInit
    create     = gitCreate
    modify     = gitModify
    retrieve   = gitRetrieve
    delete     = gitDelete
    move       = gitMove
    history    = gitHistory
    revision   = gitGetRevision
    index      = gitIndex
    diff       = gitDiff

instance SearchableFileStore GitFileStore where 
    search     = gitSearch


