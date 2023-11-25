# battleships-in-haskell

This is similar to the game of Battleship, but played on a 4x8 grid. This involves one player, the searcher trying to find the locations of three battleships hidden by the other player, the hider. The searcher continues to guess until they find all the hidden ships. Unlike Battleship™, a guess consists of three different locations, and the game continues until the exact locations of the three hidden ships are guessed in a single guess. After each guess, the hider responds with three numbers: 

1.  the number of ships exactly located;
2.  the number of guesses that were exactly one space away from a ship; and
3.  the number of guesses that were exactly two spaces away from a ship.

Each guess is only counted as its closest distance to any ship. For example if a guessed location is exactly the location of one ship and is one square away from another, it counts as exactly locating a ship, and not as one away from a ship. The eight squares adjacent to a square, including diagonally adjacent, are counted as distance 1 away. The sixteen squares adjacent to those squares are considered to be distance 2 away, as illustrated in this diagram of distances from the center square: 

||||||
|---|---|---|---|---|
| 2 | 2 | 2 | 2 | 2 |
| 2 | 1 | 1 | 1 | 2 |
| 2 | 1 | 0 | 1 | 2 |
| 2 | 1 | 1 | 1 | 2 |
| 2 | 2 | 2 | 2 | 2 |

 Of course, depending on the location of the center square, some of these locations will actually be outside the board.

 Here are some example ship locations, guesses, and the feedback provided by the hider:
 
Locations	| Guess	| Feedback
|---|---|---|
H1, B2, D3	| B3, C3, H3	| 0, 2, 1
H1, B2, D3	| B1, A2, H3	| 0, 2, 1
H1, B2, D3	| B2, H2, H1	| 2, 1, 0
A1, D2, B3	| A3, D2, H1	| 1, 1, 0
A1, D2, B3	| H4, G3, H2	| 0, 0, 0
A1, D2, B3	| D2, B3, A1	| 3, 0, 0 


The game finishes once the searcher guesses all three ship locations in a single guess (in any order), such as in the last example above. The object of the game for the searcher is to find the target with the fewest 
possible guesses.

# My approach

My approach to calculate the next guess is as follows:

```haskell
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (lastGuess, possibleTargets) guessFeedback
    = (newGuess, next)
    where
        consistentTargets = filter (\target -> feedback target lastGuess == guessFeedback) possibleTargets
        (_, newGuess) = bestGuess consistentTargets consistentTargets
        next = consistentTargets
```

1. Given the previous guess (e.g. ["B3", "D4", "H4"]) and the list of all possible remaining targets (stored in gameState)
2. Remove the targets from the list that would have given the same feedback just given from the previous guess if it was not the correct answer

This trims down the list of all remaining possible targets, as they would have given the same feedback as before - which would have been meaningless. Otherwise, the targets would be all possible combinations of targets (4960 possible, 2480 on average) which would be feasible, but very slow compared to the above.

The next step would be to remove symmetry in the problem space.

```haskell
-- Counting frequency of unique feedback combinations from a Location to other Locations
uniqueFeedback :: [Location] -> [[Location]] -> [((Int, Int, Int), Int)]
uniqueFeedback guess targets = map (\list -> (head list, length list) ) . group . sort $ [feedback target guess | target <- targets]

-- General Formula. Sum of the occurences of 
-- feedback f squared over total number of tests.
expectedAvg :: Int -> [((Int, Int, Int), Int)] -> Float
expectedAvg total feedbacks =
    sum [((fromIntegral count) ^ 2) / (fromIntegral total) | (_, count) <- feedbacks]

-- Select best guess within minimum average from remaining possible targets
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

```

The next best guess (after trimming down) is choosing the guess with the best probability of being the correct choice, using a formula for each possible guess:

$$
\sum\limits_{f\in F} \frac{count(f)^2}{T}
$$

where $F$ is set of all distinct feedbacks, for example ((3,0,0), (1,0,2), and (2,0,1)), $count(f)$ is the number of occurrences of the feedback $f$, and and $T$ is the total number of tests.

This is compared for all remaining targets against all remaining targets. The target with the lowest average number of guesses will be chosen to be the next guess.

# Guess Quality

Feedback correctness score 1.0: Passed 200 of 200 tests

Sample guess quality score 1.2: Guessed C3 F1 F3 in 5 guesses Run time = 1.212 seconds

Guess test 1 quality score 0.857142857143: Guessed G3 D3 E1 in 7 guesses Run time = 0.572 seconds

Guess test 2 quality score 0.857142857143: Guessed G3 A2 E1 in 7 guesses Run time = 2.66 seconds

Guess test 3 quality score 1.2: Guessed G3 H3 H1 in 5 guesses Run time = 0.544 seconds

Guess test 4 quality score 1.0: Guessed D1 D2 F3 in 6 guesses Run time = 0.564 seconds

Guess test 5 quality score 1.2: Guessed A1 H2 E4 in 5 guesses Run time = 2.92 seconds

Overall 94% - H1 for the project
