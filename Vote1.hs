module Vote1 where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Maybe


readVotes :: FilePath -> IO()
readVotes path =  do
    vote <- readFile path
    -- print $ stringToIntVote $ separateVotes vote
    -- print $ winnerCandidate  $ mapCandidates $ drawing $ stringToIntVote $ separateVotes vote
    let a = mapCandidates $ drawing $ stringToIntVote $ separateVotes vote
    print a
    print $ show $ winnerCandidate $  a

drawing :: Poll -> Poll
drawing p = if winner (mapCandidates p) p then p else drawing (discard (mapCandidates p) p) -- RECURSIVE


--[x | x <- iterate  drawing $ stringToIntVote $ separateVotes poll, ( not $ winner (mapCandidates x) x) ]

{--After reading the file containing votes we need to split 
the votes and candidates from each other in a way that we can
further handle the data.
This function can take the initial String result obtained after
reading the input file and builds list containing lists with candidates
which are in string format--}
separateVotes :: String -> [[String]]
separateVotes votes =  [words w | w <- lines votes]

{--This function can take the output of separateVotes and convert 
the candidates from literal to numeriacal form.--}
stringToIntVote :: [[String]] -> [[Int]] 
stringToIntVote [] = []
stringToIntVote (x:xs) = [digitToInt a | a <- map head x] : stringToIntVote xs

type Candidates = [Int]
type Poll = [Candidates]


{--Returns list with all first preferences--}
firstPreference :: Poll -> Candidates
firstPreference =  map head 




votesLeft :: Poll -> Int
votesLeft  = length 
-- spent :: Poll -> Poll
-- spent cnd = 

topCandidate :: Map Int Int -> Int 
topCandidate m  = last $ Set.elems $ Map.keysSet m

---------------------if ((topCandidate f) >= (length b)) then True else False 

winnerCandidate :: Map Int Int -> Int
winnerCandidate m = fromMaybe 1  $ Map.lookup (last $ Set.elems $ Map.keysSet m) m

winner :: Map Int Int -> Poll-> Bool 
winner w  p = topCandidate w > div (length $ firstPreference p) 2 
-- winner w  p = topCandidate w > div (length b)  (votesLeft p)
--winner w  p = topCandidate w > div (length b)  (Map.size w)
    -- where
    --     b = firstPreference p

       

discard :: Map Int Int -> Poll -> Poll
discard m p = [ x | x <- j , not (null x)]
    where
        -- j = map (filter (/= i)) p
        -- i = head $ fromMaybe [] $ Map.lookup (head $ take 1 $ Map.keys m) m
        --k = [ x | x <- j , not (null x)]
        j = map (filter (/= i)) p
        i = fromMaybe 1 h --3 //the candidate with least first votes 
        h = Map.lookup g m --Just 3 //the values that the firt key is pair with
        g = head $ take 1 $ Map.keys m -- 1 //take the first key from the Map as they are ordered and the least chosen candidate will be at first position in the Map




mapCandidates :: Poll -> Map Int Int
mapCandidates p = Map.fromList $ zip d  ([head x | x <- c]) -- 2
   where
        d =  map length c
        c =  group $ sort $ firstPreference p

--([head x | x <- c])

poll :: String 
poll = "\
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
