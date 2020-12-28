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
    -- writeFile "stvWinner.txt" ""
    -- storeWinner $ findWinners $ prepareElection vote
    let votesWinnersList = [] : prepareElection vote --  the had of the list is a list holding the current winners
    putStrLn "\nResults from Single Transfer Vote Ellectio:"
    print $ stf votesWinnersList



 ----------First Past The Post System   
    {--In this model of voting we take the first choice from each vote and the candidate with most occurances on first position
    wins the election--}
    putStr "\n"
    print $ mapCandidates $ stringToIntVote $ separateVotes vote
    putStr "Fisrt Past the Post Results: \nThe winner is: "
    print $ winnerCandidate $ mapCandidates $ stringToIntVote $ separateVotes vote

    

----------Alternative Votin System
    putStrLn "\nAlternative Voting System results: "
    let a = mapCandidates $ drawing $ stringToIntVote $ separateVotes vote
    print $ Map.assocs a
    putStr "The winner is candidate: "
    print $ winnerCandidate  a


{--Remove empty votes form the election--}
cleanEmpty :: Poll -> Poll
cleanEmpty p = [ x | x <- p , not (null x)]

----------Single Transferable vote

    --seats <- getLine 
    -- if ((readMaybe (show seats) :: Maybe Int) == Just seats) 
    --     then do
    --         let nSeats = digitToInt seats
    --         print nSeats
    --     else print 2
    --quota  = div (length $ firstPreference $ stringToIntVote $ separateVotes seats) 


    --seats <- getLine
    --quota  = div (length $ firstPreference $ stringToIntVote $ separateVotes seats)

{--Connvert the input from the read file so it can be easily handled--}
prepareElection :: String -> Poll
prepareElection election = stringToIntVote $ separateVotes election 


type Candidates = [Int]
--  *** MUST BE MORE THAN ONE AS THIS ELECTION ALGORITHM IS FOR MULTIPLE CANDIDATES
seats :: Int
seats  = 3

-- ********** once finished with the development delete this and use the one in readVotes 
{--The quota calculating formula is:
    votes /  seats to fill + 1--}
quota :: Int
quota  = div (length $ firstPreference $ stringToIntVote $ separateVotes poll) seats + 1

--[(Int,[Int])] -> [Int]

-- {--List with the current candidates having enough first preferences--}
-- findWinners :: Poll -> Candidates
-- findWinners p = takeWhile (>= quota) $ map snd countWithPref
--     where 
--         countWithPref = zip (map length groupedSortedFirstPref ) $ map head  groupedSortedFirstPref -- [(5,1),(2,2),(1,3),(4,4)]
--         groupedSortedFirstPref = group $ sort $ firstPreference p --[[1,1,1,1,1],[2,2],[3],[4,4,4,4]]

-- THE INITIAL STATEMENT IN reaVotes under SINGLE TRANSFER VOTE  SHOULD BE vote = [] : prepareElection poll



completeElection :: Poll -> Poll
completeElection p = if takenSits >= seats 
    then [head p] -- if we have enough winners the return the head of the list as it contains all the winners
    else [foldr (:) candidatesLeft $ head p] -- else combine the rest of the list with its head

    where
       candidatesLeft =  map head $ group $ sort $ firstPreference $ tail p -- list with the candidates 
       takenSits = length $ head p


keepWinners :: Poll  -> Poll
keepWinners p = (winnerCandidate (mapCandidates newWinner) : head p) : newWinner -- take the new winner and add it to the current ones, then add all this to the updated list
    where
        newWinner = findWinners competitors (length $ head p) -- the list in state with new winner available
        competitors = tail p -- remove winners from the list 


{--If we have candidate with first preferences above the quota then return it, else remove the one with least first pref --}
findWinners :: Poll -> Int -> Poll
findWinners p winners 
    | topCandidateVotes m >= quota --  || enough -- if we have top candidate or we have enough winners then return the candidate if is winner or the list with winners if we have winners
    = p
        --if enough then completeElection p else p -- if we have enough winners then return them else return the current winner and continue the election
        --p 
        -- if winners + the resto of candidates is more than the seats than carry on
    | seats < winners + length (group $ sort $ firstPreference $ tail p) 
        = findWinners (discardLeast m p) winners --  if we neither have enough winners or winner and there is more candidates than the seats we need to fill then remove the candidate with least preference so we can rerun the counting of 1st places with new 1st values in the votes
    |otherwise =  p

    where
        m = mapCandidates p
        -- enough = enoughWinners p

        
--  ** TO CREATE EMPTY FILE BEFORE STARTING THE ALGORITHM AND TO DELETE IT ONCE EXITING THE ALGORITHM
storeWinner :: Poll  -> IO()
storeWinner p = appendFile "stvWinner.txt" $ show t ++ "\n"
    where 
        t = winnerCandidate $ mapCandidates p

{--Check if we have enough winners to take all available seats--}
enoughWinners :: Poll  -> Bool   
enoughWinners p = takenSits >= seats  -- if we already fill up all the seats
    ||seats >= takenSits + length (group $ sort $ firstPreference $ tail p)   -- or if the number of all seats >= taken sit + candidates left
     
    where 
        takenSits = length $ head p
        --winners = head p

    --length winners > seats --  *** to read winners form file instead

-- checkForDiscard :: Poll -> Poll
-- checkForDiscard p =
win :: Poll -> Poll
win p = [h] : (th : t)
    where
        t = tail p
        h = head $ head p
        th = tail $ head p


stf :: Poll -> Poll
stf p = if enoughWinners p then completeElection p else stf (updateVotes $ keepWinners p)

{--Delete as many votes, containing winners as first choice, as the value of the quota. We'll need the second choice of the rest of this votes to be assigned to the other votes having this second choice as first.--}
-- if not (null takeCandidate) then discardFirst cleanedVotes else cleanedVotes
updateVotes :: Poll -> Poll  --  ** to write helper funciton that passes candidtaes to updateVotes so we clean for all obtained candidates
updateVotes p  = if enoughWinners p then p else combine --
    
    where  
        combine = head p : updated -- put back the list with winners in first position of the updates list
        updated = cleanEmpty $ map (filter (/= candidate)) bindLists --Discard the winner as does not need to be part of the competition anymore and clean from empty lists
        -- from all sublists headed by the candidate take only what is 
        --above the quota (e.g quota 5 list 12 -> take 7 list headed by by the candidate)
        bindLists = foldr (:) takeCandidate dropCandidate
        takeCandidate = drop quota $ [ x | x <- competitors , head x == candidate]
        dropCandidate = [ x | x <- competitors , head x /= candidate] -- take all sublists without the candidate as first preference
        candidate = winnerCandidate $ mapCandidates competitors -- top candidate
        competitors = tail p -- take only the candidates which are still in the competition
        -- bindLists = foldr (:) takeCandidate dropCandidate
        -- takeCandidate = drop quota $ [ x | x <- p , head x == candidate]
        -- dropCandidate = [ x | x <- p , head x /= candidate] -- take all sublists without the candidate as first preference
        -- candidate = winnerCandidate $ mapCandidates p -- top candidate

-- {--Discard the winner as does not need to be part of the competition anymore--}
-- discardFirst :: Poll -> Poll
-- discardFirst p = cleanEmpty j --take all votes witout the empty ones
--     where
--         j = map (filter (/= (winnerCandidate $ mapCandidates p))) p -- clean the votes from the top candidate as he doesn't need to be part of the competition anymore
-- ---------------------------------------------------------------------


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
