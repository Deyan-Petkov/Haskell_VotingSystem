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
    let election = prepareElection vote
-----------Single Transfer Vote

    let votes = [] : election --  the head of the list is a list holding the current winners, initially empty
    putStrLn "\n\nResults from Single Transfer Vote Election:\n"
    let quota  = (div (length election) (seats + 1))+1 -- formula for obtaining the minimum number of first preferences required for electing a candidate
    
    putStr "Ellected winners: " >> putStrLn (init $ init $ concat [show x ++ ", "| x <- head $ stf votes quota])
    --putStr "Quota " >> (putStrLn . show) quota



 ----------First Past The Post System   
    {--In this model of voting we take the first choice from each vote and the candidate with most occurances on first position
    wins the election--}
    putStrLn"\n\n\n------------------------------------------------------"
    putStrLn "-----Fisrt Past the Post Results:-----\n"
    let mapAssocs = mapCandidates election
    mapM_ putStrLn ["Candidate " ++ (show y) ++ " has " ++ (show x) ++ " votes in total" | (x,y) <- Map.assocs mapAssocs]
    putStr "The winner is candidate: "  >> putStr (show $ winnerCandidate mapAssocs)

    

-- ----------Alternative Voting System
    putStrLn"\n\n\n\n------------------------------------------------------"
    putStrLn "-----Alternative Voting System results:-----\n "
    let a = return election
    candidateVotesAssoc $ drawing a -- start the drawing and print the final results


--  ----------Borda Count Voting System
    putStrLn"\n\n\n------------------------------------------------------"
    putStrLn "-----Borda Count Voting System results:-----\n "

    let pcp = pairCandPoints election -- list with candidates in pair with achieved points
    putStrLn "First preferences:"
    assocs $ mapCandidates election -- show how many first preferences each candidate achieved

    putStr "Point For:\n" >> putStrLn "First Place:  n" >> putStrLn "Second Place: n-1" >> putStrLn "Third Place:  n-2" >> putStrLn "....\n"

    putStrLn "Achieved Points:"
    putStrLn $ unlines ["Candidate " ++ (show x) ++ ": " ++ (show y) | (x,y) <- pcp]

    let bcw = bordaCountWinner pcp -- find the winner
    
    putStrLn $ "The winner is candidate: " ++ show (snd bcw) ++ " with " ++ show (fst bcw) ++ " points :\n"


--  ----------Contingent Voting System
    putStrLn"\n\n\n------------------------------------------------------"
    putStrLn "-----Contingent Voting System results:----- \n"
    let cv = contingentVoting election -- returns the winner
    let cnd = mapCandidates election -- creates pairs of candidate and his first preferences 
    let haveWinner = winner cnd election --Bool
    let quota = length election

    putStr "Votes in the poll: " >> putStrLn (show quota)
    putStr "The quota is: " >> (putStrLn.show) (div (length election) 2)
    putStr "\n"
    assocs cnd --statistics about candidates and their firs preferences

    putStr "We have winner with majority votes: " >> putStrLn (show haveWinner)
    if haveWinner then
        putStrLn $ "The winner is candidate: " ++ (show $ snd cv) ++ "\nwith " ++ (show $ fst cv) ++ " votes."
        else 
            do
                let discarded = Map.elems $ last $ take 3 $ iterate Map.deleteMax cnd -- remove the two candidates with most first preferences
                putStrLn "Then discard all candidates but the two having most first preferences.\n"
                putStr "Discarding candidates: " 
                mapM_ putStr ([(show x) ++ ", " | x <- (init discarded)]) >> putStrLn (show $ last discarded) --print all candidates which will be discarded
                
                putStrLn "First preferences:"
                assocs $ mapCandidates $ keepWinners election discarded

                putStrLn $ "The winner is candidate: " ++ (show $ snd cv) ++ " with " ++ (show $ fst cv) ++ " votes."

    putStrLn "\n\n\n------------------------------------------------------"






---------Functions:

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


{--Prints out each candidate and his/hers first preferences--}
assocs :: Map Int Int -> IO ()
assocs m = do 
    mapM_ putStrLn ["Candidate " ++ (show y) ++ ": " ++ (show x) | (x,y) <- Map.assocs m]
    putStr "\n"

{--Prints statistics. Used mainly for the alternative voting system--}
candidateVotesAssoc :: IO Poll -> IO ()
candidateVotesAssoc p = do
    a <- p
    let m = mapCandidates a

    putStrLn $ "Votes Left: " ++ show (length a)
    putStrLn "First preferences:"

    if winner m a then do
        assocs m
        putStrLn $ "Candidate " ++ show (winnerCandidate  m) ++ " is selected.\n"
        else
        assocs m
    


{--Connvert the input from the read file so it can be easily handled--}
prepareElection :: String -> Poll
prepareElection elctn = stringToIntVote $ separateVotes elctn 


{--Remove empty votes form the election--}
cleanEmpty :: Poll -> Poll
cleanEmpty p = [ x | x <- p , not (null x)]



----------Single Transferable vote
{--We have seats preset to 3 --} 
seats :: Int
seats  = 3


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
addWinner :: Poll  -> Int -> Poll
addWinner p quota = (winnerCandidate (mapCandidates newWinner) : head p) : newWinner {--take the new winner and add it to the current ones, 
                                                                                  then add all this to the updated with findWinners list--}
    where
        newWinner = findWinners competitors (length $ head p) quota-- the list in state with new winner available
        competitors = tail p -- remove winners from the list 


{--If we have candidate with first preferences above the quota then return it, else remove the one with least first pref 
   The second parameter (Int) holds the current number of winners--}
findWinners :: Poll -> Int -> Int -> Poll
findWinners p winners quota 
    | topCandidateVotes m >= quota = p -- if we have top candidate then return the list as it is

        -- if winners + the rest of candidates is more than the seats than carry on, there is no threat of running out of candidates
    | seats < winners + length (group $ sort $ firstPreference $ tail p) 
        = findWinners (discardLeast m p) winners quota
    |otherwise =  p  {--if seats are equal to or more than winners + candidates left, then return the list 
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



{--Delete as many votes containing the winner as first choice, as the value of the quota. 
   We'll need the second choice of the rest of this votes so we can improove the results for the rest of the candidates 
   which eventually will give us new winner.--}
updateVotes :: Poll -> Int -> Poll 
updateVotes p quota = if enoughWinners p then p else head p : (cleanEmpty $ map (filter (/= candidate)) $ foldr (:) (drop (quota) [ x | x <- competitors , head x == candidate])  [ x | x <- competitors , head x /= candidate])
                    --if enoughWinners p then p else combine 
    
    where  
        -- combine = head p : updated -- put back the list with winners in first position of the updates list
        -- updated = cleanEmpty $ map (filter (/= candidate)) bindLists --Discard the winner as does not need to be part of the competition anymore and clean from empty sublists
        -- bindLists = foldr (:) takeCandidate dropCandidate -- the new state of the election list
        -- takeCandidate = drop (quota ) $ [ x | x <- competitors , head x == candidate] {--take all votes with the top candidate as first preference but remove as many of 
        --     these votes as the quota(the rest of the votes will help other candidates achieve majority after removing the current winner from first place)--}
        -- dropCandidate = [ x | x <- competitors , head x /= candidate] -- take all sublists without the candidate as first preference
        candidate = winnerCandidate $ mapCandidates competitors -- top candidate
        competitors = tail p -- take only the candidates which are still in the competition and ommit the winners


{--Single Transfer Vote will recursively call findWinners, addWinner and updateVotes untill enoughWiners is satisfied--}
stf :: Poll -> Int -> Poll
stf p quota = if enoughWinners p then completeElection p else stf (updateVotes (addWinner p quota) quota) quota



----------------------Alternative Voting System

{--iterates over the election discarding candidates until one of them obtain majority--}
drawing :: IO Poll -> IO Poll
drawing p = do
    a <- p
    let m = mapCandidates a
    if winner m a then do -- if we have candidate with enough first preferences then return the poll as it is
        p
        else do
        candidateVotesAssoc p --print statistics
        putStrLn $ "Candidate " ++ show (leastCandidate m) ++ " is eliminated.\n"

        let discarded = return $ discardLeast m a --discard the candidate with least first preferences and return the updated Poll

        drawing discarded -- repeat the drawing with updated list of votes



{--Returns list with all first preferences--}
firstPreference :: Poll -> Vote
firstPreference =  map head 

{--returns the count of votes for the most elected candidate--}
topCandidateVotes :: Map Int Int -> Int 
topCandidateVotes m  = last $ Set.elems $ Map.keysSet m

{--extract the candidate with least first preferences from Maybe--}
leastCandidate :: Map Int Int -> Int
leastCandidate m = fromMaybe 1 h 
    where
        h = Map.lookup g m --Just # //the "name" of the candidate that the first/smallest key is pair with
        g = head $ take 1 $ Map.keys m -- 1 //take the first key from the Map as they are ordered and the least chosen candidate will be at first position in the Map
       -- m = mapCandidates p


{--returns the winner--}
winnerCandidate :: Map Int Int -> Int
winnerCandidate m = fromMaybe 1  $ Map.lookup (last $ Set.elems $ Map.keysSet m) m


{--returns true if one of the candidates has been chosen as first
    preference more times than the half of the number of votes in the election--}
winner :: Map Int Int -> Poll-> Bool 
winner w  p = topCandidateVotes w > div (length $ firstPreference p) 2 


{--remove the candidate with least first preferences
    in the votes and eventually clean the list from empty votes--}
discardLeast :: Map Int Int -> Poll -> Poll
discardLeast m p = cleanEmpty j --take all votes witout the empty ones
    where
        j = map (filter (/= leastCandidate m)) p -- clean the votes from the candidate with least first preferences 



{--Takes the election list as a parameter and returns map structure with pairs of candidate and sum of achieved first preferences--}
mapCandidates :: Poll -> Map Int Int
mapCandidates p = Map.fromList $ zip d  ([head x | x <- c]) 
   where
        d =  map length c -- list containing the count of occurrances for each candidate in first position [5,2,1,4]
        c =  group $ sort $ firstPreference p {--list which contains lists with the candidates 
long as much as the number of times each of them appeared in the election [[1,1,1,1,1],[2,2],[3],[4,4,4,4]] --} 


-- ============================  Borda Count Voting System
    

{--Find the number of candidates in the election--}
candidatesCount :: Poll -> Int
candidatesCount p = maximum $ map length p

{--We take a Poll and return a list containing lists with pairs of the candidate and his/hers points (according to prefference position).--}
setPoints :: Poll -> [[(Int,Int)]]
setPoints x = [zip a [candCnt, (candCnt - 1)..1] | a <- x]
--[[(4,4),(3,2),(2,3),(1,1)]...]
    where 
        candCnt =  candidatesCount x



{--returns winner candidate paired with achieved points--}
bordaCountWinner :: [(Int, Int)]  -> (Int, Int)
bordaCountWinner p = last $ sort $ [(y,x) | (x,y) <-  p]
                -- j 
    -- where
    --     i = [(y,x) | (x,y) <- pairCandPoints]  - reverse candidate and points in each pair
    --     j = last $ sort i  -- returns pair of the points and the winner candidate


{--zip candidate and its total points according to preference position--}
pairCandPoints :: Poll -> [(Int, Int)]
pairCandPoints p = zip ([fst $ head x | x <- pointPerVote]) $ map sum $ map (\z -> [y | (x,y) <- z]) pointPerVote  
                    --h
    where
        pointPerVote = map (\z-> [(x,y) | (x,y) <- (sort $ concat $ setPoints p), x == z]) [1..(candidatesCount p)] {--list containing lists with pairs of the candidate and its points for single vote --}

        -- c = sort $ concat $ setPoints p  -- list with sorted pairs of candidates and their points for each vote
        -- d = map (\z-> [(x,y) | (x,y) <- c, x == z]) [1,2,3,4]  -- list containing lists with pairs of the candidate and its points for single vote
        -- e = map (\z -> [y | (x,y) <- z]) d  -- list with the points for each candidate
        -- f = map sum e  -- list with total points for each candidate (1,2,3...)
        -- g = [fst $ head x | x <- d]   -- list with the candidates in order given in d
        -- h = zip g f   -- zip candidate and its total points according to preference position



-- ============================  Contingent Voting System

{--If none of the candidates achieved majority after the initial counting of first 
   preferences then remove all but the two leading candidates from the election, and count the first preferences again with the new state of the votes.--}
contingentVoting :: Poll -> (Int,Int)
contingentVoting p 
    | winner a p = Map.findMax a --returns the winner if we have majority
    | otherwise = Map.findMax $ mapCandidates $ keepWinners p (Map.elems $ last $ take 3 $ iterate Map.deleteMax a) -- g
    where
        a = mapCandidates p -- put the candidates in pair with their number of first preference
        -- b = winner a p -- True/False
        -- c = take 3 $ iterate Map.deleteMax a -- delete the 2 most chosen candidates as first preference
        -- d = Map.elems $ last c -- all candidates except the 2 current winners
        -- e = keepWinners p d -- discard all candidates except the 2 winners
        -- f = mapCandidates e -- put the candidates in pair with their number of first preference
        -- g = Map.findMax f -- returns the winner(if we have 2 winners with same number of first preferences will return one of them, but this is hard to happen in real election. We can handle this returning 0 (asking for new voting) if f size is 1)


{--discard all candidates in the Poll which are in the Int list. The Int list should contain all candidates except the two with most first preferences. So this function should return the two most chosen candidates.--}
keepWinners :: Poll -> [Int] -> Poll
keepWinners p (w:xw) = keepWinners [filter (/= w) x | x <- p] xw
keepWinners p [] = cleanEmpty p




{--input1.txt (from the testing files)--}
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
