#!/usr/bin/env runghc

This program runs tests for the filestore modules.
Invoke it with:

    runghc Tests.hs

> import Data.FileStore.Git as Git
> import Test.HUnit
> import System.Directory (removeDirectoryRecursive)
> import Data.Maybe (isJust)
> import Control.Monad (forM)
> import Prelude hiding (catch)
> import Control.Exception (catch)

> testFileStore :: (FileStore, String) -> IO Counts 
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
> testContents = "Test contents\nSecond line."

> testTitle :: String
> testTitle = "New resource.txt"

> initializeTest fs = TestCase $ do
>   initialize fs
>   ind <- index fs
>   assertEqual "index of just-initialized repository" ind []
>   catch (initialize fs >> assertFailure "did not return error for existing repository") $
>     \e -> assertEqual "error status from existing repository" e RepositoryExists

> createTest fs = TestCase $ do
>   create fs testTitle testAuthor "description of change" testContents
>   mbRev <- latest fs testTitle
>   assertBool "latest returns a revision after create" (isJust mbRev) 

> retrieveTest fs = TestCase $ do
>   mbRev <- latest fs testTitle
>   case mbRev of
>     Nothing     -> assertFailure "latest did not return a revision"
>     Just rev    -> do
>       (rev', cont) <- retrieve fs "New resource.txt" Nothing
>       assertEqual "revision returned by retrieve" rev rev'
>       assertEqual "contents returned by retrieve" testContents cont 

> main = do
>   let fileStores = [(gitFileStore "tmp/gitfs", "Data.FileStore.Git")]
>   forM fileStores testFileStore
>   removeDirectoryRecursive "tmp"

