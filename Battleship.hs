-- 1245550 Lok Chow
-- Complete implementation of all functions for Project 2, including testing framework.

{-
The approach taken to calculate the nextGuess is: Given the previous guess
and list of all possible targets, remove the targets that would give the
same feedback that was just given from the previous guess. This trims down
the remaining list of possible targets, as they would have given the same
feedback as before.
Moreover, symmetry is removed in the problem space. The next best guess (after
trimming down) is choosing the guess with the best probability of being the
correct choice, using a formula for each possible guess. This is compared
for all remaining targets against all remaining targets. The target with the
lowest average number of guesses will be chosen to be the next guess.
-}

module Battleships (Location, toLocation, fromLocation, feedback, startTest,
                GameState, initialGuess, nextGuess) where

import Data.List

type Column = Char
type Row = Int
data Location = Location Column Row
    deriving (Show, Eq, Ord)

-- list of remaining possible targets
type GameState = ([[Location]])

-- Distance from one Location to another by Chebyshev distance
distanceFrom :: Location -> Location -> Int
distanceFrom (Location tgt_col tgt_row) (Location gss_col gss_row)
    | distance == 0 = (-1)
    | distance > 2 = 0
    | otherwise = distance
    where
        distance = max (abs (fromEnum tgt_col - fromEnum gss_col)) (abs (tgt_row - gss_row))

-- | Gives Just the Location named by the string,
-- or Nothing if the string is not a valid location name
toLocation :: String -> Maybe Location
toLocation (col:row)
    | elem col ['A'..'H'] && elem (read row) [1..4] = Just (Location col (read row))
    | otherwise = Nothing

-- | Gives the two-character string of the specified Location for any location
fromLocation :: Location -> String
fromLocation (Location col row) = [col] ++ show row

-- | Takes a target and a guess, and returns the appropriate feedback
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback targets guesses = foldl add (0,0,0) xs
    where
        distances = [distanceFrom tgt gss | tgt <- targets, gss <- guesses] 
        xs = map (minimize . processGuess) [a,b,c]
        a = [distances !! 0, distances !! 3, distances !! 6]
        b = [distances !! 1, distances !! 4, distances !! 7]
        c = [distances !! 2, distances !! 5, distances !! 8]

-- | Adds 3 tuples of Ints together
add:: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add (a,b,c) (d,e,f) = (a+d, b+e, c+f)

-- | Takes output the distance of a guess from the three ships,
-- and returns the number of times that: the guess is exactly on a ship,
-- the guess is one tile away from a ship, the guess is two tiles away
-- in a 3-tuple of Ints
processGuess :: [Int] -> (Int, Int, Int)
processGuess xs = 
    (length $ filter (==(-1)) xs,
    length $ filter (==1) xs, 
    length $ filter (==2) xs)

-- | Takes output of processGuess and outputs the closest
-- distance of the guess to any ship - only counting 1 for each guess.
minimize :: (Int, Int, Int) -> (Int, Int, Int)
minimize (a,b,c)
    | a /= 0 = (1, 0, 0)
    | b /= 0 = (0, 1, 0)
    | c /= 0 = (0, 0, 1)
    | otherwise = (0, 0, 0)

-- | List of all tiles
allLocations :: [Location]
allLocations = [(Location x y) | x <- ['A'..'H'], y <- [1..4]]

-- | Generates list of all guesses, as 3 elements per list
allGuesses :: [[Location]]
allGuesses = [[a,b,c] | a <- allLocations, b <- allLocations, c <- allLocations, a<b, b<c]

-- | First guess is a corner of the board. This maximizes information returned
-- for the first guess.
initialGuess :: ([Location], GameState)
initialGuess = ( [ (Location 'A' 1), (Location 'A' 2), (Location 'B' 1) ] , 
                allGuesses )

-- | Takes a guess, the gamestate, and the guess' feedback
-- Returns next guess and new gamestate
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (lastGuess, possibleTargets) guessFeedback
    = (newGuess, next)
    where
        -- Hint 3: Filter out other guesses that would have given the same feedback
        consistentTargets = filter (\target -> feedback target lastGuess == guessFeedback) possibleTargets
        (_, newGuess) = bestGuess consistentTargets consistentTargets
        next = consistentTargets 

-- | Hint 6: Counting frequency of unique feedback combinations from a Location to other Locations
uniqueFeedback :: [Location] -> [[Location]] -> [((Int, Int, Int), Int)]
uniqueFeedback guess targets = map (\list -> (head list, length list) ) . group . sort $ [feedback target guess | target <- targets]

-- | Hint 6: General Formula outlined in the spec. Sum of the occurences of 
-- feedback f squared over total number of tests.
expectedAvg :: Int -> [((Int, Int, Int), Int)] -> Float
expectedAvg total feedbacks =
    sum [((fromIntegral count) ^ 2) / (fromIntegral total) | (_, count) <- feedbacks]

-- | Hint 6: Select best guess within minimum average from remaining possible targets
bestGuess :: [[Location]] -> [[Location]] -> (Float, [Location])
-- no possible target, make this cost infinity
bestGuess _ [x] = (1/0, x)
bestGuess (x:xs) targets
    | null xs = (expected, x)
    | expected < expected' = (expected, x)
    | otherwise = (expected', x')
    where
        -- expected number of guesses on average that guess x would take.
        expected = expectedAvg (length targets) (uniqueFeedback x targets)
        -- If there is a guess with a better average, then do bestGuess again.
        -- Worst case is O(n^2).
        (expected', x') = if null xs then (expected, x) else bestGuess xs targets

------------------------- TESTING --------------------------------

-- | Find average of X testCases starting from Yth element from all possible targets
startTest :: Int -> Int -> Int
startTest x y = average [main2 $ testCasesToString x | x <- (take x . drop y $ allGuesses)]

testCasesToString :: [Location] -> String
testCasesToString (x:y:z:_) = fromLocation x ++ " " ++ fromLocation y ++ " " ++ fromLocation z

average :: [Int] -> Int
average xs = sum xs `div` length xs

-- | Repurposed from Main.hs - returns number of guesses a game took.
main2 :: String -> Int
main2 xs = do
  case mapM toLocation $ words xs of
    Just target@[_,_,_] ->
      Battleshipstest2 target
      
-- | Repurposed from Main.hs
Battleshipstest2 :: [Location] -> Int
Battleshipstest2 target = do
  let (guess,other) = initialGuess
  loop2 target guess other 1

-- | Repurposed from Main.hs
loop2 :: [Location] -> [Location] -> Battleships.GameState -> Int -> Int
loop2 target guess other guesses = do
  let answer = feedback target guess
  if answer == (3,0,0) then guesses
  else do
      let (guess',other') = nextGuess (guess,other) answer
      loop2 target guess' other' (guesses+1)