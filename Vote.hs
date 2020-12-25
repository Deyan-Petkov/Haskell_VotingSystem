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
    print $ stringToIntVote $ separateVotes vote


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

leastPreference :: Poll -> Int
--leastPreference cnd = head $ fromMaybe [] $ Map.lookup $ head $ take 1 $ Map.keys $ Map.fromList $ zip (map length ) group $ sort $ firstPreference cnd
leastPreference cnd = i
    where
        i = head $ fromMaybe  [] h --3 //the candidate with least first votes
        h = Map.lookup g f --Just [3] //the values that the firt key is pair with
        g = head $ take 1 $ Map.keys f -- 1 //take the first key from the Map as they are ordered and the least chosen candidate will be at first position in the Map
        f = Map.fromList e --[(1,[3]),(2,[2,2])..
        e = zip d c --[(4,[1,1,1,1]),(2,[2,2])..
        d = map length c --[4,2,1,4]
        c = group $ sort b --[[1,1,1,1],[2,2],[3],[4,4,4,4]]
        b = firstPreference a --[4,2,4,4,1,1,2,3,1,4,1]
        a = cnd    --[[4,2,3,1],[2,3,4,1],... list with all the votes


{--
    a = all votes
    b =  firstPref a
    c = group $ sort b
    d = map length c
    --e = create set for each list
    e = zip d c
    map each set to its size as a k  //places the smallest key(length of accurances of candidate) on first place in the map so we just need to find the value of the smalles k)
    f =  Map.fromList e
    find the smallest/first k 
       g = head $ take 1 $ Map.keys f
    take the value of this k
       h =  Map.lookup g f
    take the k form Maybe
        i = head $ fromMaybe [] h


case result of
     Just a -> putStrLn $ "I'm so happy you chose "++show a++"."
     Nothing -> putStrLn $ "So sorry; "++input++" is not a valid option."

    --}





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
