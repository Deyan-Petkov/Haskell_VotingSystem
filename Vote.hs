module Vote where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char

--[b : [] | b <- a]  -- a is (lines vote)

--readVotes :: FilePath -> IO[[String]]
readVotes :: FilePath -> IO()
readVotes path =  do
    vote <- readFile path
    print $ stringToIntVote $ separateVotes vote
    --print vote
   -- print  [words b | b <- lines vote] 
--[["4","3","2","1"],["1","2","3","4"],["1","3","2","4"]]
{--
["4","3","2","1"]
*Vote> [digitToInt a | a <- map head e]
[4,3,2,1]
--}

--[digitToInt  a | a <- (map head e)]
--map snd $ zip [1..] c

separateVotes :: String -> [[String]]
separateVotes votes =  [words w | w <- lines votes]


stringToIntVote :: [[String]] -> [[Int]] 
stringToIntVote [x] = []
stringToIntVote (x:xs) = [digitToInt a | a <- map head x] : stringToIntVote xs

vote :: String 
vote = "\
    \4 2 3 1\n\
    \2 3 4 1\n\
    \4 2 1 3\n\
    \4 3 2 1\n\
    \1 2 3 4\n\
    \1 2 3\n\
    \2 4 3\n\
    \3 2\n\
    \1 3 4 2\n\
    \4 3 2 1\n\
    \1 2 3 4\n\
    \1 3 2 4\n"
