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

    initialize     :: b -> IO ()
    save           :: Contents a => b -> ResourceName -> Author -> String -> a -> IO ()
    create         :: Contents a => b -> ResourceName -> Author -> String -> a -> IO ()
    modify         :: Contents a => b -> ResourceName -> RevisionId -> Author -> String -> a -> IO (Either MergeInfo ())
    retrieve       :: Contents a => b -> ResourceName -> Maybe RevisionId -> IO a
    delete         :: b -> ResourceName -> Author -> String -> IO ()
    move           :: b -> ResourceName -> Author -> String -> IO ()
    history        :: b -> [ResourceName] -> TimeRange -> IO History
    revision       :: b -> ResourceName -> Maybe RevisionId -> IO Revision
    index          :: b -> IO [ResourceName]
    diff           :: b -> ResourceName -> RevisionId -> RevisionId -> IO String
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

