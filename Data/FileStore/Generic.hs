{- |
   Module      : Data.FileStore.Generic
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : GHC 6.10 required

   Generic functions for "Data.FileStore".
-}

module Data.FileStore.Generic
           ( modify
           , create
           , DI(..)
           , diff
           , searchRevisions
           , smartRetrieve
           )

where
import Data.FileStore.Types

import Control.Exception (throwIO, catch, SomeException, try)
import Data.FileStore.Utils
import Data.Maybe (isNothing)
import Data.List (isInfixOf)
import qualified Data.List.Split as S (whenElt, split)
import Data.Char (isSpace)
import Data.Algorithm.Diff (DI(..), getGroupedDiff)
import Prelude hiding (catch)

handleUnknownError :: SomeException -> IO a
handleUnknownError = throwIO . UnknownError . show

-- | Like save, but first verify that the resource name is new.  If not, throws a 'ResourceExists'
-- error.
create :: Contents a
       => FileStore
       -> ResourceName      -- ^ Resource to create.
       -> Author            -- ^ Author of change.
       -> String            -- ^ Description of change.
       -> a                 -- ^ Contents of resource.
       -> IO ()
create fs name author logMsg contents = catch (latest fs name >> throwIO ResourceExists)
                                                (\e -> if e == NotFound
                                                 then save fs name author logMsg contents
                                                 else throwIO e)

-- | Modify a named resource in the filestore.  Like save, except that a revision ID
-- must be specified.  If the resource has been modified since the specified revision,
-- @Left@ merge information is returned.  Otherwise, @Right@ the new contents are saved.  
modify  :: Contents a
        => FileStore
        -> ResourceName      -- ^ Resource to create.
        -> RevisionId        -- ^ ID of previous revision that is being modified.
        -> Author            -- ^ Author of change.
        -> String            -- ^ Description of change.
        -> a                 -- ^ Contents of resource.
        -> IO (Either MergeInfo ())
modify fs name originalRevId author msg contents = do
  latestRevId <- latest fs name
  latestRev <- revision fs latestRevId
  if idsMatch fs originalRevId latestRevId
     then save fs name author msg contents >> return (Right ())
     else do
       latestContents <- retrieve fs name (Just latestRevId)
       originalContents <- retrieve fs name (Just originalRevId)
       (conflicts, mergedText) <- catch 
                                  (mergeContents ("edited", toByteString contents) (originalRevId, originalContents) (latestRevId, latestContents))
                                  handleUnknownError
       return $ Left (MergeInfo latestRev conflicts mergedText)

-- | Split a string at spaces (but include the spaces in the list of results).
splitOnSpaces :: String -> [String]
splitOnSpaces = S.split (S.whenElt isSpace) 

-- | Return a unified diff of two revisions of a named resource, using an external @diff@
-- program.
diff :: FileStore
     -> ResourceName      -- ^ Resource name to get diff for.
     -> Maybe RevisionId  -- ^ @Just@ old revision ID, or @Nothing@ for empty.
     -> Maybe RevisionId  -- ^ @Just@ oew revision ID, or @Nothing@ for latest.
     -> IO [(DI, [String])]
diff fs name id1 id2 = do
  contents1 <- if isNothing id1
                  then return ""
                  else retrieve fs name id1
  contents2 <- retrieve fs name id2
  let words1 = splitOnSpaces contents1
  let words2 = splitOnSpaces contents2
  return $ getGroupedDiff words1 words2

-- | Return a list of all revisions that are saved with the given
-- description or with a part of this description.
searchRevisions :: FileStore
                -> ResourceName      -- ^ The resource to search history for.
                -> Description       -- ^ Revision description to search for.
                -> Bool              -- ^ When true the description must
                                     --   match exactly, when false partial
                                     --   hits are allowed.
                -> IO [Revision]

searchRevisions repo name desc exact = do
  let matcher = if exact
                then (== desc)
                else (desc `isInfixOf`)
  revs <- (history repo) [name] (TimeRange Nothing Nothing)
  return $ Prelude.filter (matcher . revDescription) revs

-- | Try to retrieve a resource from the repository by name and possibly a
-- revision identifier. When retrieving a resource by revision identifier fails
-- this function will try to fetch the latest revision for which the
-- description matches the given string.
smartRetrieve
  :: Contents a
  => FileStore
  -> ResourceName    -- ^ Resource name to retrieve.
  -> Maybe String    -- ^ @Just@ revision ID or description, or @Nothing@ for empty.
  -> Bool            -- ^ @True@ for exact description match, @False@ for partial match.
  -> IO a
smartRetrieve fs name mrev exact = do
  edoc <- try (retrieve fs name mrev)
  case (edoc, mrev) of
    
    -- Regular retrieval using revision identifier succeeded, use this doc.
    (Right doc, _) -> return doc

    -- Retrieval of latest revision failed, nothing we can do about this.
    (Left e, Nothing) -> throwIO (e :: FileStoreError)

    -- Retrieval failed, we can try fetching a revision by the description.
    (Left _, Just rev) -> do
      revs <- searchRevisions fs name rev exact
      if Prelude.null revs

        -- No revisions containing this description.
        then throwIO NotFound

        -- Retrieve resource for latest matching revision.
        else retrieve fs name (Just $ revId $ Prelude.head revs)

