{- A script demonstrating how to use filestore to query a Darcs repository,
   count how many edits were made on each day, and print out the data in a
   headerless 'date,patch count' CSV format.

Example usage:

$ runhaskell darcs-days.hs wiki/ > foo.csv

Output looks like this:

2008-09-26,6
2008-09-29,1
2008-10-03,1
2008-10-04,3
2008-10-09,2
...
-}
import Data.FileStore (darcsFileStore, history, revDateTime, TimeRange(..))
import System.Environment (getArgs)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (showGregorian)
import Data.Map (fromListWith, toList)

main :: IO ()
main = do args <- fmap head getArgs

          let fs = darcsFileStore args
          changes <- history fs [] (TimeRange Nothing Nothing) Nothing

          let dates = fmap (utctDay . revDateTime) changes
          let counts = toList $ fromListWith (+) [(d, 1::Int) | d <- dates]

          -- print in a CSV style
          mapM_ (\(date,count) -> putStrLn (showGregorian date ++ "," ++ show count)) counts
