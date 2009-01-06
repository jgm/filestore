{-# LANGUAGE Rank2Types #-}
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
           , TimeRange
           , FileStoreError(..)
           , FileStore(..)
           , DateTime ) 
where
import Data.ByteString.Lazy (ByteString)
import Data.DateTime (DateTime)

type RevisionId = String

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
  } deriving (Show, Read, Eq)

class Contents a where
  fromByteString :: ByteString -> a
  toByteString   :: a -> ByteString

type History = [(ResourceName, Revision)]

type TimeRange = (Maybe DateTime, Maybe DateTime)

data FileStoreError = Merged Revision Bool String  -- latest revision conflicts? merged-text
                    | AlreadyExists
                    | NotFound
                    | Unchanged
                    | UnknownError String
                    deriving (Show)

data FileStore =
  FileStore {
    create         :: Contents a => ResourceName -> Author -> String -> a -> IO (Either FileStoreError ())
  , modify         :: Contents a => ResourceName -> RevisionId -> Author -> String -> a -> IO (Either FileStoreError ())
  , retrieve       :: Contents a => ResourceName -> Maybe RevisionId -> IO (Either FileStoreError (Revision, a))
  , delete         :: ResourceName -> Author -> String -> IO (Either FileStoreError ())
  , move           :: ResourceName -> Author -> String -> IO (Either FileStoreError ())
  , history        :: [ResourceName] -> TimeRange -> IO History
  , latest         :: ResourceName -> IO (Maybe Revision)
  , index          :: IO [ResourceName]
  , search         :: [String] -> IO [(ResourceName, [String])]
  , diff           :: ResourceName -> RevisionId -> RevisionId -> IO (Either FileStoreError String)
  }

