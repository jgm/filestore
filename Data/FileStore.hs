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
    create         :: ResourceName -> Author -> String -> ByteString -> IO (Either FileStoreError ())
  , modify         :: ResourceName -> RevisionId -> Author -> String -> ByteString -> IO (Either FileStoreError ())
  , retrieve       :: ResourceName -> Maybe RevisionId -> IO (Either FileStoreError (Revision, ByteString))
  , delete         :: ResourceName -> Author -> String -> IO (Either FileStoreError ())
  , move           :: ResourceName -> Author -> String -> IO (Either FileStoreError ())
  , history        :: [ResourceName] -> TimeRange -> IO History
  , latest         :: ResourceName -> IO (Maybe Revision)
  , index          :: IO [ResourceName]
  , search         :: [String] -> IO [(ResourceName, [String])]
  , diff           :: ResourceName -> RevisionId -> RevisionId -> IO (Either FileStoreError String)
  }

