#!/usr/bin/env runghc

This program runs tests for the filestore modules.
Invoke it with:

    runghc Tests.hs

> import Data.FileStore
> import Test.HUnit
> import System.Directory (removeDirectoryRecursive)
> import Data.Maybe (isJust)
> import Control.Monad (forM)
> import Prelude hiding (catch)
> import Control.Exception (catch)
> import Data.DateTime

> testFileStore :: FileStore a => (a, String) -> IO Counts 
> testFileStore (fs, fsName) = do
>   putStrLn $ "**********************************"
>   putStrLn $ "Testing " ++ fsName
>   putStrLn $ "**********************************"
>   runTestTT $ TestList $ map (\(label, testFn) -> TestLabel label $ testFn fs) 
>     [ ("initialize", initializeTest)
>     , ("create resource", createTest1)
>     , ("create resource in subdirectory", createTest2)
>     , ("create resource with unicode name", createTest3)
>     , ("retrieve resource", retrieveTest1)
>     , ("retrieve resource in a subdirectory", retrieveTest2)
>     , ("retrieve resource with unicode name", retrieveTest3)
>     , ("modify resource", modifyTest)
>     ]

> testAuthor :: Author
> testAuthor = Author "Test Suite" "suite@test.org"

> testContents :: String
> testContents = "Test contents.\nSecond line.\nThird test line with some unicode αβ."

> testTitle :: String
> testTitle = "New resource.txt"

> subdirTestTitle :: String
> subdirTestTitle = "subdir/Subdir title.txt"

> unicodeTestTitle :: String
> unicodeTestTitle = "αβγ"

Initialize a repository, check for empty index, and then try to initialize again
in the same directory (should raise an error):

> initializeTest fs = TestCase $ do
>   initialize fs
>   ind <- index fs
>   assertEqual "index of just-initialized repository" ind []
>   catch (initialize fs >> assertFailure "did not return error for existing repository") $
>     \e -> assertEqual "error status from existing repository" e RepositoryExists

Create a resource, and check to see that revision returns a revision for it:

> createTest1 fs = TestCase $ do
>   create fs testTitle testAuthor "description of change" testContents
>   rev <- revision fs testTitle Nothing
>   assertBool "revision returns a revision after create" ((not . null . revId) rev)

Create a resource in a subdirectory, and check to see that revision returns a revision for it:

> createTest2 fs = TestCase $ do
>   create fs subdirTestTitle testAuthor "description of change" testContents
>   rev <- revision fs subdirTestTitle Nothing
>   assertBool "revision returns a revision after create" ((not . null . revId) rev)

Create a resource with a unicode title, and check to see that revision returns a revision for it:

> createTest3 fs = TestCase $ do
>   create fs unicodeTestTitle testAuthor "description of change" testContents
>   rev <- revision fs unicodeTestTitle Nothing
>   assertBool "revision returns a revision after create" ((not . null . revId) rev)

Retrieve latest version of a resource:

> retrieveTest1 fs = TestCase $ do
>   cont <- retrieve fs testTitle Nothing
>   assertEqual "contents returned by retrieve" testContents cont 

Retrieve latest version of a resource (in a subdirectory):

> retrieveTest2 fs = TestCase $ do
>   cont <- retrieve fs subdirTestTitle Nothing
>   assertEqual "contents returned by retrieve" testContents cont 

Retrieve latest version of a resource with a unicode name:

> retrieveTest3 fs = TestCase $ do
>   cont <- retrieve fs unicodeTestTitle Nothing
>   assertEqual "contents returned by retrieve" testContents cont 

Modify a resource.  Retrieve the contents and make sure they were changed.  Try
modifying again with the old revision as base.  This should produce a merge.
Check to make sure the merge is correct. Try modifying with the old revision
as base, but with the same contents as the new revision.  This should produce
a merge with no conflicts.  Finally, modify with the new revision, which
should succeed without a merge.

Modify a resource:

> modifyTest fs = TestCase $ do

  Modify a resource.  Should return Right ().

>   rev <- revision fs testTitle Nothing
>   let modifiedContents = unlines $ take 2 $ lines testContents
>   modResult <- modify fs testTitle (revId rev) testAuthor "removed third line" modifiedContents
>   assertEqual "results of modify" (Right ()) modResult

  Now retrieve the contents and make sure they were changed.

>   modifiedContents' <- retrieve fs testTitle Nothing
>   newRev <- revision fs testTitle Nothing
>   assertEqual "retrieved contents after modify" modifiedContents' modifiedContents

  Now try to modify again, using the old revision as base.  This should result in a merge with conflicts.

>   modResult2 <- modify fs testTitle (revId rev) testAuthor "modified from old version" (testContents ++ "\nFourth line")
>   let normModResult2 = Left (MergeInfo {mergeRevision = newRev, mergeConflicts = True, mergeText = "Test contents.\nSecond line.\n<<<<<<< edited\nThird test line with some unicode \945\946.\nFourth line\n=======\n>>>>>>> " ++ revId newRev ++ "\n"})
>   assertEqual "results of modify from old version" normModResult2 modResult2

  Now try it again, still using the old version as base, but with contents of the new version.
  This should result in a merge without conflicts.

>   modResult3 <- modify fs testTitle (revId rev) testAuthor "modified from old version" modifiedContents
>   let normModResult3 = Left (MergeInfo {mergeRevision = newRev, mergeConflicts = False, mergeText = modifiedContents})
>   assertEqual "results of modify from old version with new version's contents" normModResult3 modResult3

  Now try modifying again, this time using the new version as base. Should succeed with Right ().

>   modResult4 <- modify fs testTitle (revId newRev) testAuthor "modified from new version" (modifiedContents ++ "\nThird line")
>   assertEqual "results of modify from new version" (Right ()) modResult4 

> main = do
>   let fileStores = [(GitFileStore { gitRepoPath = "tmp/gitfs"}, "Data.FileStore.Git")]
>   forM fileStores testFileStore
>   removeDirectoryRecursive "tmp"

