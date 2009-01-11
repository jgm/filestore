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

> main = do
>   let fileStores = [(GitFileStore { gitRepoPath = "tmp/gitfs"}, "Data.FileStore.Git")]
>   forM fileStores testFileStore
>   removeDirectoryRecursive "tmp"

> testFileStore :: FileStore a => (a, String) -> IO Counts 
> testFileStore (fs, fsName) = do
>   putStrLn $ "**********************************"
>   putStrLn $ "Testing " ++ fsName
>   putStrLn $ "**********************************"
>   runTestTT $ TestList $ map (\(label, testFn) -> TestLabel label $ testFn fs) 
>     [ ("initialize", initializeTest)
>     , ("create resource", createTest1)
>     , ("create resource in subdirectory", createTest2)
>     , ("create resource with non-ascii name", createTest3)
>     , ("retrieve resource", retrieveTest1)
>     , ("retrieve resource in a subdirectory", retrieveTest2)
>     , ("retrieve resource with non-ascii name", retrieveTest3)
>     , ("modify resource", modifyTest)
>     , ("delete resource", deleteTest)
>     , ("rename resource", renameTest)
>     , ("test for matching IDs", matchTest)
>     , ("test for history and revision", historyTest)
>     ]

> testAuthor :: Author
> testAuthor = Author "Test Suite" "suite@test.org"

> testContents :: String
> testContents = "Test contents.\nSecond line.\nThird test line with some Greek αβ."

> testTitle :: String
> testTitle = "New resource.txt"

> subdirTestTitle :: String
> subdirTestTitle = "subdir/Subdir title.txt"

> nonasciiTestTitle :: String
> nonasciiTestTitle = "αβγ"

*** Initialize a repository, check for empty index, and then try to initialize again
*** in the same directory (should raise an error):

> initializeTest fs = TestCase $ do
>   initialize fs
>   ind <- index fs
>   assertEqual "index of just-initialized repository" ind []
>   catch (initialize fs >> assertFailure "did not return error for existing repository") $
>     \e -> assertEqual "error status from existing repository" e RepositoryExists

*** Create a resource, and check to see that revision returns a revision for it:

> createTest1 fs = TestCase $ do
>   create fs testTitle testAuthor "description of change" testContents
>   rev <- revision fs testTitle Nothing
>   assertBool "revision returns a revision after create" ((not . null . revId) rev)

*** Create a resource in a subdirectory, and check to see that revision returns a revision for it:

> createTest2 fs = TestCase $ do
>   create fs subdirTestTitle testAuthor "description of change" testContents
>   rev <- revision fs subdirTestTitle Nothing
>   assertBool "revision returns a revision after create" ((not . null . revId) rev)

*** Create a resource with a non-ascii title, and check to see that revision returns a revision for it:

> createTest3 fs = TestCase $ do
>   create fs nonasciiTestTitle testAuthor "description of change" testContents
>   rev <- revision fs nonasciiTestTitle Nothing
>   assertBool "revision returns a revision after create" ((not . null . revId) rev)

*** Retrieve latest version of a resource:

> retrieveTest1 fs = TestCase $ do
>   cont <- retrieve fs testTitle Nothing
>   assertEqual "contents returned by retrieve" testContents cont 

*** Retrieve latest version of a resource (in a subdirectory):

> retrieveTest2 fs = TestCase $ do
>   cont <- retrieve fs subdirTestTitle Nothing
>   assertEqual "contents returned by retrieve" testContents cont 

*** Retrieve latest version of a resource with a nonascii name:

> retrieveTest3 fs = TestCase $ do
>   cont <- retrieve fs nonasciiTestTitle Nothing
>   assertEqual "contents returned by retrieve" testContents cont 

*** Modify a resource:

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
>   let normModResult2 = Left (MergeInfo {mergeRevision = newRev, mergeConflicts = True, mergeText =
>                         "Test contents.\nSecond line.\n<<<<<<< edited\nThird test line with some Greek \945\946.\nFourth line\n=======\n>>>>>>> " ++
>                         revId newRev ++ "\n"})
>   assertEqual "results of modify from old version" normModResult2 modResult2

    Now try it again, still using the old version as base, but with contents of the new version.
    This should result in a merge without conflicts.

>   modResult3 <- modify fs testTitle (revId rev) testAuthor "modified from old version" modifiedContents
>   let normModResult3 = Left (MergeInfo {mergeRevision = newRev, mergeConflicts = False, mergeText = modifiedContents})
>   assertEqual "results of modify from old version with new version's contents" normModResult3 modResult3

    Now try modifying again, this time using the new version as base. Should succeed with Right ().

>   modResult4 <- modify fs testTitle (revId newRev) testAuthor "modified from new version" (modifiedContents ++ "\nThird line")
>   assertEqual "results of modify from new version" (Right ()) modResult4 

*** Delete a resource:

> deleteTest fs = TestCase $ do

    Create a file and verify that it's there.

>   let toBeDeleted = "Aaack!"
>   create fs toBeDeleted testAuthor "description of change" testContents
>   ind <- index fs
>   assertBool "index contains resource to be deleted" (toBeDeleted `elem` ind) 

    Now delete it and verify that it's gone.

>   delete fs toBeDeleted testAuthor "goodbye"
>   ind <- index fs
>   assertBool "index does not contain resource that was deleted" (not (toBeDeleted `elem` ind))

*** Rename a resource:

> renameTest fs = TestCase $ do

    Create a file and verify that it's there.

>   let oldName = "Old Name"
>   let newName = "newdir/New Name.txt"
>   create fs oldName testAuthor "description of change" testContents
>   ind <- index fs
>   assertBool "index contains old name" (oldName `elem` ind) 
>   assertBool "index does not contain new name" (newName `notElem` ind)

    Now rename it and verify that it changed names.

>   rename fs oldName newName testAuthor "rename"
>   ind <- index fs
>   assertBool "index does not contain old name" (oldName `notElem` ind) 
>   assertBool "index contains new name" (newName `elem` ind)

    Try renaming a file that doesn't exist.

>   catch (rename fs "nonexistent file" "other name" testAuthor "rename" >>
>       assertFailure "rename of nonexistent file did not throw error") $
>       \e -> assertEqual "error status from rename of nonexistent file" NotFound e

*** Test history and revision

> historyTest fs = TestCase $ do

    Get history for three files

>   hist <- history fs [testTitle, subdirTestTitle, nonasciiTestTitle] (TimeRange Nothing Nothing)
>   assertBool "history is nonempty" (not (null hist))
>   now <- getCurrentTime
>   rev <- revision fs testTitle Nothing  -- get latest revision
>   assertBool "history contains latest revision" (rev `elem` hist)
>   assertEqual "revAuthor" testAuthor (revAuthor rev)
>   assertBool "revId non-null" (not (null (revId rev)))
>   assertBool "revDescription non-null" (not (null (revDescription rev)))
>   assertEqual "revModified" [testTitle] (revModified rev)
>   let revtime = revDateTime rev
>   histNow <- history fs [testTitle] (TimeRange (Just now) Nothing)
>   assertBool "history from now onwards is empty" (null histNow)

*** Test diff

*** Test search

*** Test IDs match

> matchTest fs = TestCase $ do

>   assertBool "match with two identical IDs" (idsMatch fs "abcde" "abcde")
>   assertBool "match with nonidentical but matching IDs" (idsMatch fs "abcde" "abcde5553")
>   assertBool "non-match" (not (idsMatch fs "abcde" "abedc"))

