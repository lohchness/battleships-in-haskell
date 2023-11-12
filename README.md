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


The game finishes once the searcher guesses all three ship locations in a single guess (in any order), such as in the last example above. The object of the game for the searcher is to find the target with the fewest possible guesses.

# Guess Quality

Feedback correctness score 1.0: Passed 200 of 200 tests
Sample guess quality score 1.2: Guessed C3 F1 F3 in 5 guesses Run time = 1.212 seconds
Guess test 1 quality score 0.857142857143: Guessed G3 D3 E1 in 7 guesses Run time = 0.572 seconds
Guess test 2 quality score 0.857142857143: Guessed G3 A2 E1 in 7 guesses Run time = 2.66 seconds
Guess test 3 quality score 1.2: Guessed G3 H3 H1 in 5 guesses Run time = 0.544 seconds
Guess test 4 quality score 1.0: Guessed D1 D2 F3 in 6 guesses Run time = 0.564 seconds
Guess test 5 quality score 1.2: Guessed A1 H2 E4 in 5 guesses Run time = 2.92 seconds

Overall 94% - H1 for the project
