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

> testFileStore :: FileStore a => (a, String) -> IO Counts 
> testFileStore (fs, fsName) = do
>   putStrLn $ "**********************************"
>   putStrLn $ "Testing " ++ fsName
>   putStrLn $ "**********************************"
>   runTestTT $ TestList $ map (\(label, testFn) -> TestLabel label $ testFn fs) 
>     [ ("initialize", initializeTest)
>     , ("create resource", createTest)
>     , ("retrieve resource", retrieveTest)
>     ]

> testAuthor :: Author
> testAuthor = Author "Test Suite" "suite@test.org"

> testContents :: String
> testContents = "Test contents.\nSecond line.\nThird test line with some unicode αβ."

> testTitle :: String
> testTitle = "New resource.txt"

Initialize a repository, check for empty index, and then try to initialize again
in the same directory (should raise an error):

> initializeTest fs = TestCase $ do
>   initialize fs
>   ind <- index fs
>   assertEqual "index of just-initialized repository" ind []
>   catch (initialize fs >> assertFailure "did not return error for existing repository") $
>     \e -> assertEqual "error status from existing repository" e RepositoryExists

Create a resource, and check to see that revision returns a revision for it:

> createTest fs = TestCase $ do
>   create fs testTitle testAuthor "description of change" testContents
>   rev <- revision fs testTitle Nothing
>   assertBool "revision returns a revision after create" ((not . null . revId) rev)

Retrieve a resource (latest version):

> retrieveTest fs = TestCase $ do
>   cont <- retrieve fs "New resource.txt" Nothing
>   assertEqual "contents returned by retrieve" testContents cont 

Modify...

Retrieve earlier version...and latest version...

> main = do
>   let fileStores = [(GitFileStore { gitRepoPath = "tmp/gitfs"}, "Data.FileStore.Git")]
>   forM fileStores testFileStore
>   removeDirectoryRecursive "tmp"

