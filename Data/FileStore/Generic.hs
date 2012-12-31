{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Data.FileStore.Generic
   Copyright   : Copyright (C) 2009 John MacFarlane, Gwern Branwen, Sebastiaan Visser
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : GHC 6.10 required

   Generic utility functions for working with filestores.
-}

module Data.FileStore.Generic
           ( modify
           , create
           , Diff(..)
           , diff
           , searchRevisions
           , smartRetrieve
           , richDirectory
           )

where
import Data.FileStore.Types

import Control.Exception (throwIO, catch, SomeException, try)
import Data.FileStore.Utils
import Data.List (isInfixOf)
import Data.Algorithm.Diff (Diff(..), getGroupedDiff)
import System.FilePath ((</>))
import Prelude hiding (catch)

handleUnknownError :: SomeException -> IO a
handleUnknownError = throwIO . UnknownError . show

-- | Like save, but first verify that the resource name is new.  If not, throws a 'ResourceExists'
-- error.
create :: Contents a
       => FileStore
       -> FilePath          -- ^ Resource to create.
       -> Author            -- ^ Author of change.
       -> Description       -- ^ Description of change.
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
        -> FilePath          -- ^ Resource to create.
        -> RevisionId        -- ^ ID of previous revision that is being modified.
        -> Author            -- ^ Author of change.
        -> Description       -- ^ Description of change.
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

-- | Return a unified diff of two revisions of a named resource.
-- Format of the diff is a list @[(Diff, [String])]@, where
-- @DI@ is @F@ (in first document only), @S@ (in second only),
-- or @B@ (in both), and the list is a list of lines (without
-- newlines at the end).
diff :: FileStore
     -> FilePath      -- ^ Resource name to get diff for.
     -> Maybe RevisionId  -- ^ @Just@ old revision ID, or @Nothing@ for empty.
     -> Maybe RevisionId  -- ^ @Just@ oew revision ID, or @Nothing@ for latest.
     -> IO [Diff [String]]
diff fs name Nothing id2 = do
  contents2 <- retrieve fs name id2
  return [Second (lines contents2) ]   -- no need to run getGroupedDiff here - diff vs empty document 
diff fs name id1 id2 = do
  contents1 <- retrieve fs name id1
  contents2 <- retrieve fs name id2
  return $ getGroupedDiff (lines contents1) (lines contents2)

-- | Return a list of all revisions that are saved with the given
-- description or with a part of this description.
searchRevisions :: FileStore
                -> Bool              -- ^ When true the description must
                                     --   match exactly, when false partial
                                     --   hits are allowed.
                -> FilePath          -- ^ The resource to search history for.
                -> Description       -- ^ Revision description to search for.
                -> IO [Revision]

searchRevisions repo exact name desc = do
  let matcher = if exact
                then (== desc)
                else (desc `isInfixOf`)
  revs <- history repo [name] (TimeRange Nothing Nothing) Nothing
  return $ Prelude.filter (matcher . revDescription) revs

-- | Try to retrieve a resource from the repository by name and possibly a
-- revision identifier. When retrieving a resource by revision identifier fails
-- this function will try to fetch the latest revision for which the
-- description matches the given string.
smartRetrieve
  :: Contents a
  => FileStore
  -> Bool            -- ^ @True@ for exact description match, @False@ for partial match.
  -> FilePath        -- ^ Resource name to retrieve.
  -> Maybe String    -- ^ @Just@ revision ID or description, or @Nothing@ for empty.
  -> IO a
smartRetrieve fs exact name mrev = do
  edoc <- try (retrieve fs name mrev)
  case (edoc, mrev) of
    
    -- Regular retrieval using revision identifier succeeded, use this doc.
    (Right doc, _) -> return doc

    -- Retrieval of latest revision failed, nothing we can do about this.
    (Left e, Nothing) -> throwIO (e :: FileStoreError)

    -- Retrieval failed, we can try fetching a revision by the description.
    (Left _, Just rev) -> do
      revs <- searchRevisions fs exact name rev
      if Prelude.null revs

        -- No revisions containing this description.
        then throwIO NotFound

        -- Retrieve resource for latest matching revision.
        else retrieve fs name (Just $ revId $ Prelude.head revs)

-- | Like 'directory', but returns information about the latest revision.
richDirectory :: FileStore -> FilePath -> IO [(Resource, Either String Revision)]
richDirectory fs fp = directory fs fp >>= mapM f
  where f r = Control.Exception.catch (g r) (\(e :: FileStoreError)-> return ( r, Left . show $ e ) )
        g r@(FSDirectory _dir) = return (r,Left "richDirectory, we don't care about revision info for directories")
        g res@(FSFile file) = do rev <- revision fs =<< latest fs ( fp </> file )
                                 return (res,Right rev)

