{-# LANGUAGE TypeSynonymInstances, Rank2Types, DeriveDataTypeable #-}
{- Abstract interface to a versioned file store, which could be
-  implemented using a VCS or a database.

   (C) 2008 John MacFarlane
   Based on ideas from Network.Orchid.Core.Backend.
-}

module Data.FileStore
           ( RevisionId
           , ResourceName
           , Author(..)
           , Revision(..)
           , Contents(..)
           , History
           , TimeRange(..)
           , MergeInfo(..)
           , FileStoreError(..)
           , FileStore(..)
           , SearchMatch(..)
           , DateTime ) 
where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Data.DateTime (DateTime)
import Data.Typeable
import Control.Exception

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

type History = [(ResourceName, Revision)]

data TimeRange =
  TimeRange {
    trFrom :: Maybe DateTime
  , trTo   :: Maybe DateTime
  } deriving (Show, Read, Eq)

data MergeInfo =
  MergeInfo {
    mergeRevision  :: Revision
  , mergeConflicts :: Bool
  , mergeText      :: String
  } deriving (Show, Read, Eq, Typeable)

data FileStoreError =
    Merged MergeInfo
  | AlreadyExists
  | NotFound
  | Unchanged
  | UnknownError String
  deriving (Show, Read, Eq, Typeable)

instance Exception FileStoreError

data SearchMatch =
  SearchMatch {
    matchResourceName :: ResourceName
  , matchLineNumber   :: Integer
  , matchLine         :: String
  } deriving (Show, Read, Eq)

data FileStore =
  FileStore {
    create         :: Contents a => ResourceName -> Author -> String -> a -> IO ()
  , modify         :: Contents a => ResourceName -> RevisionId -> Author -> String -> a -> IO ()
  , retrieve       :: Contents a => ResourceName -> Maybe RevisionId -> IO (Revision, a)
  , delete         :: ResourceName -> Author -> String -> IO ()
  , move           :: ResourceName -> Author -> String -> IO ()
  , history        :: [ResourceName] -> TimeRange -> IO History
  , latest         :: ResourceName -> IO (Maybe Revision)
  , index          :: IO [ResourceName]
  , search         :: [String] -> IO [SearchMatch]
  , diff           :: ResourceName -> RevisionId -> RevisionId -> IO String
  }
