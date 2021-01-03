module Vote1 where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Maybe
import Text.Read

readVotes :: FilePath -> IO()
readVotes path =  do
    vote <- readFile path 

-----------Single Transfer Vote

    let votes = [] : prepareElection vote --  the head of the list is a list holding the current winners, initially empty
    putStrLn "\nResults from Single Transfer Vote Ellectio:"
    print $ stf votes



 ----------First Past The Post System   
    {--In this model of voting we take the first choice from each vote and the candidate with most occurances on first position
    wins the election--}
    putStr "\n"
    print $ mapCandidates $ prepareElection vote
    putStr "Fisrt Past the Post Results: \nThe winner is: "
    print $ winnerCandidate $ mapCandidates $ prepareElection vote

    

----------Alternative Votin System
    putStrLn "\nAlternative Voting System results: "
    let a = mapCandidates $ drawing $ prepareElection vote
    print $ Map.assocs a
    putStr "The winner is candidate: "
    print $ winnerCandidate  a



---------Functions:

{--Connvert the input from the read file so it can be easily handled--}
prepareElection :: String -> Poll
prepareElection election = stringToIntVote $ separateVotes election 


{--Remove empty votes form the election--}
cleanEmpty :: Poll -> Poll
cleanEmpty p = [ x | x <- p , not (null x)]


----------Single Transferable vote
{--We have seats preset to 3 --} -- 
seats :: Int
seats  = 3


{--The quota calculating formula is:
   votes /  seats to fill + 1--}
-- quota :: Poll -> Int
-- quota  p = div (length $ firstPreference p) seats + 1
quota :: Int
quota  = div (length $ firstPreference $ stringToIntVote $ separateVotes poll) seats + 1

{--Take the final state of the election and output the winners taking the seats--}
completeElection :: Poll -> Poll
completeElection p = if takenSits >= seats 
    then [head p] -- if we have enough winners then return the head of the list as it contains all the winners
    else [foldr (:) candidatesLeft $ head p] {--else combine the rest of the list with candidates with its head 
                                                to obtain all candidates which will take a seat.--}

    where
       candidatesLeft =  map head $ group $ sort $ firstPreference $ tail p -- list with the candidates 
       takenSits = length $ head p -- how many winners we have up to now


{--Add winner to the list with current winners (head of the Poll--}
addWinner :: Poll  -> Poll
addWinner p = (winnerCandidate (mapCandidates newWinner) : head p) : newWinner {--take the new winner and add it to the current ones, 
                                                                                  then add all this to the updated with findWinners list--}
    where
        newWinner = findWinners competitors (length $ head p) -- the list in state with new winner available
        competitors = tail p -- remove winners from the list 


{--If we have candidate with first preferences above the quota then return it, else remove the one with least first pref 
   The second parameter (Int) holds the current number of winners--}
findWinners :: Poll -> Int -> Poll
findWinners p winners 
    | topCandidateVotes m >= quota = p -- if we have top candidate then return it

        -- if winners + the rest of candidates is more than the seats than carry on, there is no threat of running out of candidates
    | seats < winners + length (group $ sort $ firstPreference $ tail p) 
        = findWinners (discardLeast m p) winners
    |otherwise =  p {--if seats are equal to or more than winners + candidates left, then return the list 
                       as we cannot afford to delete more candidates--}

    where
        m = mapCandidates p -- map candidates to the number of times each appear as first preference in the votes



{--Returns False if we still need to fill up seats or the number of seats is smaller than the winners + candidates left
   If enoughWinners return True we should stop looking for winners as if we remove one more candidate with least 1st preferences 
    then we would not have enough to fill up the seats.--}
enoughWinners :: Poll  -> Bool   
enoughWinners p = takenSits >= seats  -- if we already filled up all the seats
    ||seats >= takenSits + length (group $ sort $ firstPreference $ tail p)-- or if we have just enough winners + condidates left to fill up the seats
     
    where 
        takenSits = length $ head p -- winners are stored in the head of the list


{--Single Transfer Vote will recursively call findWinners, addWinner and updateVotes untill enoughWiners is satisfied--}
stf :: Poll -> Poll
stf p = if enoughWinners p then completeElection p else stf (updateVotes $ addWinner p)


{--Delete as many votes containing the winner as first choice, as the value of the quota. 
   We'll need the second choice of the rest of this votes so we can improove the results for the rest of the candidates 
   which eventually will give us new winner.--}
updateVotes :: Poll -> Poll 
updateVotes p  = if enoughWinners p then p else combine --
    
    where  
        combine = head p : updated -- put back the list with winners in first position of the updates list
        updated = cleanEmpty $ map (filter (/= candidate)) bindLists --Discard the winner as does not need to be part of the competition anymore and clean from empty sublists
        bindLists = foldr (:) takeCandidate dropCandidate -- the new state of the election list
        takeCandidate = drop (quota ) $ [ x | x <- competitors , head x == candidate] {--take all votes with the top candidate as first preference but remove as many of 
            these votes as the quota(the rest of the votes will help other candidates achieve majority after removing the current winner from first place)--}
        dropCandidate = [ x | x <- competitors , head x /= candidate] -- take all sublists without the candidate as first preference
        candidate = winnerCandidate $ mapCandidates competitors -- top candidate
        competitors = tail p -- take only the candidates which are still in the competition and ommit the winners


----------------------Alternative Voting System

{--iterates over the election discarding candidates until one of them obtain majority--}
drawing :: Poll -> Poll
drawing p = if winner m p then p else drawing (discardLeast m p)
    where
        m = mapCandidates p

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

leastCandidate :: Map Int Int -> Int
leastCandidate m = fromMaybe 1 h --extract the candidate with least first preferences from Maybe
    where
        h = Map.lookup g m --Just # //the "name" of the candidate that the first/smallest key is pair with
        g = head $ take 1 $ Map.keys m -- 1 //take the first key from the Map as they are ordered and the least chosen candidate will be at first position in the Map
       -- m = mapCandidates p

{--returns the winner--}
winnerCandidate :: Map Int Int -> Int
winnerCandidate m = fromMaybe 1  $ Map.lookup (last $ Set.elems $ Map.keysSet m) m

{--returns true if we one of the candidates has been chosen as first
    preference more times than the half of the number of votes in the election--}
winner :: Map Int Int -> Poll-> Bool 
winner w  p = topCandidateVotes w > div (length $ firstPreference p) 2 


{--remove the candidate with least first preferences
    in the votes and eventually clean the list from empty votes--}
discardLeast :: Map Int Int -> Poll -> Poll
discardLeast m p = cleanEmpty j --take all votes witout the empty ones
    where
        j = map (filter (/= leastCandidate m)) p -- clean the votes from the candidate with least first preferences 

{--list which contains lists with the candidates 
long as much as the number of times each of them appeared in the election [[1,1,1,1,1],[2,2],[3],[4,4,4,4]] --}  
-- groupSortFirstPref :: Poll -> Poll
-- groupSortFirstPref p = group $ sort $ firstPreference p

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
