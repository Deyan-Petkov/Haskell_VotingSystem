module Vote1 where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Maybe


-- *Vote1> c
-- [(1,3),(2,2),(4,4),(5,1)]
-- *Vote1> d
-- ["Candidate",": "]
-- *Vote1> putStr (head d) >>  putStrLn (" " ++ ( show $ snd $ head c))
-- Candidate 3
-- *Vote1>

readVotes :: FilePath -> IO()
readVotes path =  do
    vote <- readFile path
    -- print $ stringToIntVote $ separateVotes vote
    -- print $ winnerCandidate  $ mapCandidates $ drawing $ stringToIntVote $ separateVotes vote
    let a = mapCandidates $ drawing $ stringToIntVote $ separateVotes vote
    --print a
    print $ Map.assocs a
    print $ show $ winnerCandidate $  a

{--iterates over the election discarding candidates until one of them obtain majority--}
drawing :: Poll -> Poll
drawing p = if winner (mapCandidates p) p then p else drawing (discard (mapCandidates p) p) 

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
--stringToIntVote (x:xs) = [digitToInt a | a <- map head x] : stringToIntVote xs
stringToIntVote xs
  = map (\ x -> [digitToInt a | a <- map head x]) xs

type Vote = [Int] -- a single vote is represented by list of candidates
type Poll = [Vote]


{--Returns list with all first preferences--}
firstPreference :: Poll -> Vote
firstPreference =  map head 

{--returns the count of votes for the most elected candidate--}
topCandidateVotes :: Map Int Int -> Int 
topCandidateVotes m  = last $ Set.elems $ Map.keysSet m

{--returns the winner--}
winnerCandidate :: Map Int Int -> Int
winnerCandidate m = fromMaybe 1  $ Map.lookup (last $ Set.elems $ Map.keysSet m) m

{--returns true if some of the candidates have been chosen more
    times than the ha--}
winner :: Map Int Int -> Poll-> Bool 
winner w  p = topCandidateVotes w > div (length $ firstPreference p) 2 

{--remove the candidate with least first preferences
    in the votes and eventually clean the list from empty votes--}
discard :: Map Int Int -> Poll -> Poll
discard m p = [ x | x <- j , not (null x)] --take all votes witout the empty ones
    where
        j = map (filter (/= i)) p -- clean the votes from the candidate with least first preferences 
        i = fromMaybe 1 h --extract the candidate with least first preferences from Maybe
        h = Map.lookup g m --Just # //the "name" of the candidate that the first/smallest key is pair with
        g = head $ take 1 $ Map.keys m -- 1 //take the first key from the Map as they are ordered and the least chosen candidate will be at first position in the Map

{----}
mapCandidates :: Poll -> Map Int Int
mapCandidates p = Map.fromList $ zip d  ([head x | x <- c]) -- zip the count of first occurrances for each candidate in first position with the candidate and put the pair in a map list
   where
        d =  map length c -- list containing the count of occurrances for each candidate in first position [5,2,1,4]
        c =  group $ sort $ firstPreference p {--list which contains lists with the candidates 
        long as much as the number of times each of them appeared in the election [[1,1,1,1,1],[2,2],[3],[4,4,4,4]] --}




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
