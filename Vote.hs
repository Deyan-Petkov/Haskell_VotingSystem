module Vote where

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
    print $ iterate leastPreference $ stringToIntVote $ separateVotes vote

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
stringToIntVote [x] = []
stringToIntVote (x:xs) = [digitToInt a | a <- map head x] : stringToIntVote xs

type Candidates = [Int]
type Poll = [Candidates]

-- votesToSets :: [Vote] -> [[Set Vote]]
-- votesToSets vote = map Set.fromList vote

{--Returns list with all first preferences--}
firstPreference :: Poll -> Candidates
firstPreference p =  map head p

-- leastPreference :: Poll -> Int
-- leastPreference cnd = i
leastPreference :: Poll -> Poll
leastPreference cnd = k
    where
        -- n = [Set.elems x | x <- m] -- convert the Set back to List
        -- m =  [ x | x <- l, not ( Set.null x) ]
        -- l = [Set.difference x k | x <- j] -- the Set without the least candidate
        -- k = Set.singleton i -- convert the least candidate into Set 
        -- j = map Set.fromList cnd --conver the list into Set so we can search more efficiently
        k = [ x | x <- j , not (null x)]
        j = map (filter (/= i)) cnd
        i = head $ fromMaybe  [] h --3 //the candidate with least first votes
        h = Map.lookup g f --Just [3] //the values that the firt key is pair with
        g = head $ take 1 $ Map.keys f -- 1 //take the first key from the Map as they are ordered and the least chosen candidate will be at first position in the Map
        f = Map.fromList e --[(1,[3]),(2,[2,2])..
        e = zip d c --[(4,[1,1,1,1]),(2,[2,2])..
        d = map length c --[4,2,1,4]
        c = group $ sort b --[[1,1,1,1],[2,2],[3],[4,4,4,4]]
        b = firstPreference a --[4,2,4,4,1,1,2,3,1,4,1]
        a = cnd    --[[4,2,3,1],[2,3,4,1],... list with all the votes

-- spent :: Poll -> Poll
-- spent cnd = 

-- discard :: Int -> Poll -> Poll
-- discard candidate fromPoll = m
--     where 
--         j = map Set.fromList fromPoll
--         k = Set.singleton candidate
--         l = [Set.difference x k | x <- j]
--         m = [Set.elems x | x <- l]


{--
let j = map Set.fromList a
let k = Set.singleton i
let l = [Set.difference x k | x <- j]
let m = [ Set.elems x | x <- l]
--}


--takeWhile (not . null) $ iterate leastPreference $ stringToIntVote $ separateVotes poll


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
