# Parallel_2048
A 2048 solver in Haskell using expectiminimax algorithm with alpha-beta pruning and parallelizing the search

# Game
2048 is played on a 4x4 grid with numbered tiles which can slide up, down, left, or right. This game can be
modeled as a two player game, in which the computer AI generates a 2- or 4-tile placed randomly on the board,
and the player then selects a direction to move the tiles. The tiles move until they either (1) collide with
another tile, or (2) collide with the edge of the grid. If two tiles of the same number collide in a move, they merge
into a single tile valued at the sum of the two originals. The resulting tile cannot merge with another tile again in
the same move.

# Goal
An adversarial search agent to play the 2048-puzzle game. The process continues until the game is over; that is, until no further legal moves can be made. At the end of the game, the maximum tile value on the board is displayed.

## Implementation
Implemented the **expectiminimax** algorithm for adversarial search (a variation of Minimax algorithm). Note that 90% of tiles placed by the computer are 2’s, while the remaining 10% are 4’s.  
**Alpha-beta pruning** which should speed up the search process by eliminating irrelevant branches.  
**Heuristic functions and weights:** After trying many different hueristics and weights I found a combination of heuristics such as Weight Matrix, Available Cells, Monotonicity, each weighted to achieve a highers score.  
**Parallelizing:** adding parallelizem to a specific depth, and using 4 cores reduced search time by almost 75%
