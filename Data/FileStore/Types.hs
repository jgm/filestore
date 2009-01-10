{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}
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

