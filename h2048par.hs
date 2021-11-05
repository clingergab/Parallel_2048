{-

h2048
A Haskell implementation of 2048.

Gregor Ulm

last update:
2014-06-18

Please consult the file README for further information
on this program.

Our work: Added parallel minimax solver with alpha-beta pruning

-}

import Prelude hiding (Left, Right)
--import Data.Char (toLower)
import Data.List
import System.IO
import System.Random
import Text.Printf
import Control.Parallel.Strategies


data Move = Up | Down | Left | Right
type Grid = [[Int]]

start :: IO Grid
start = do grid'  <- addTile $ replicate 4 [0, 0, 0, 0]
           addTile grid'

merge :: [Int] -> [Int]
merge xs = merged ++ padding
    where padding = replicate (length xs - length merged) 0
          merged  = combine $ filter (/= 0) xs
          combine (x:y:ys) | x == y = x * 2 : combine ys
                           | otherwise = x  : combine (y:ys)
          combine x = x

move :: Move -> Grid -> Grid
move Left  = map merge
move Right = map (reverse . merge . reverse)
move Up    = transpose . move Left  . transpose
move Down  = transpose . move Right . transpose

getZeroes :: Grid -> [(Int, Int)]
getZeroes grid = filter (\(row, col) -> (grid!!row)!!col == 0) coordinates
    where singleRow n = zip (replicate 4 n) [0..3]
          coordinates = concatMap singleRow [0..3]

setSquare :: Grid -> (Int, Int) -> Int -> Grid
setSquare grid (row, col) val = pre ++ [mid] ++ post
    where pre  = take row grid
          mid  = take col (grid!!row) ++ [val] ++ drop (col + 1) (grid!!row)
          post = drop (row + 1) grid

isMoveLeft :: Grid -> Bool
isMoveLeft grid = sum allChoices > 0
    where allChoices = map (length . getZeroes . flip move grid) directions
          directions = [Left, Right, Up, Down]

getChildren :: Grid -> [Grid]
getChildren grid = filter (\x -> x /= grid) [move d grid | d <- directions]
    where directions = [Left, Right, Up, Down]


printGrid :: Grid -> IO ()
printGrid grid = do
    --putStr "\ESC[2J\ESC[2J\n" -- clears the screen
    putStrLn ""
    mapM_ (putStrLn . showRow) grid

showRow :: [Int] -> String
showRow = concatMap (printf "%5d")

--moves :: [(Char, Move)]
--moves = keys "wasd" ++ keys "chtn"
    --where keys chars = zip chars [Up, Left, Down, Right]

--captureMove :: IO Move
--captureMove = do
    --inp <- getChar
    --case lookup (toLower inp) moves of
        --Just x  -> return x
       -- Nothing -> do putStrLn "Use WASD or CHTN as input"
                     -- captureMove

check2048 :: Grid -> Bool
check2048 grid = [] /= filter (== 2048) (concat grid)
                
addTile :: Grid -> IO Grid
addTile grid = do
    let candidates = getZeroes grid
    pick <- choose candidates
    val  <- choose [2,2,2,2,2,2,2,2,2,4]
    let new_grid = setSquare grid pick val
    return new_grid

choose :: [a] -> IO a
choose xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

--newGrid :: Grid -> IO Grid
--newGrid grid = do
    --let m = Left --getAIMove grid
    --let new_grid = move m grid
    --return new_grid

sumOfTiles :: Grid -> Integer
sumOfTiles grid = toInteger $ sum $ map sum grid


weightMatrix :: Grid -> Integer
weightMatrix grid = sumOfTiles $ zipWith (zipWith (*)) matrix grid
    --where matrix = [[1073741824, 268435456, 67108864, 16777216],[65536,262144,1048576,4194304],[16384,4096,1024,256],[1,4,16,64]]   
    --where matrix = [[1073741824, 268435456, 67108864, 16777216],[4194304,1048576,262144,65536],[16384,4096,1024,256],[64,16,4,1]]
    --where matrix = [[7,6,5,4],[6,5,4,3],[5,4,3,2],[4,3,2,1]] 
    where matrix = if maxTile grid <= 512 then [[21,8,3,3],[9,5,2],[4,3]] else  [[19,9,5,3],[8,4,2],[3]]
    --where matrix = [[26000,,22,20],[12,14,16,18],[10,8,6,4],[1,2,3,4]]

monotonicity :: Grid -> Int -> Integer
monotonicity [] _ = 0
monotonicity (x:xs) currentValue = fromIntegral (monotonicityHelper x currentValue) + monotonicity xs currentValue

monotonicityHelper :: [Int] -> Int -> Int
monotonicityHelper [] _ = 0
monotonicityHelper (x:xs) currentValue 
                | x < currentValue = 1 + monotonicityHelper xs x 
                | otherwise = monotonicityHelper xs x 

smoothness :: Grid -> Integer
smoothness [] = 0
smoothness (x:xs) = smoothnessHelper x + smoothness xs 

smoothnessHelper :: [Int] -> Integer
smoothnessHelper [] = 0
smoothnessHelper (a:b:c:d:_) = fromIntegral $ ((abs (a - b)) + (abs (b - c)) + (abs (c - d))) * 5
smoothnessHelper [_] = error "wrong number of elements"
smoothnessHelper [_,_] = error "wrong number of elements"
smoothnessHelper [_,_,_] = error "wrong number of elements"
                


availableCells :: Grid -> Integer
availableCells grid = toInteger $ sum $ map zeros grid
    where zeros l = length $ filter (\x -> x == 0) l

weWon :: Grid -> Integer
weWon grid 
    | maxTile grid == 2048 = 999999
    | otherwise = 0


utility :: Grid -> Integer
utility grid = weightMatrix grid + (100 * availableCells grid) + (15 * monotonicity grid 9999) + (15 * monotonicity (transpose grid) 9999) - (smoothness grid) - (smoothness (transpose grid)) + weWon grid
--utility grid = fromIntegral $ (3 * monotonicity grid 9999) + (3 * monotonicity (transpose grid) 9999) - (smoothness grid) - (smoothness (transpose grid)) + (maxTile grid) + (fromIntegral $ 3 * availableCells grid) + (fromIntegral $ sumOfTiles grid)


parMaximize :: Grid -> Integer -> Integer -> Int -> Grid
parMaximize grid a b maxDepth
  | maxDepth == 0 || not (isMoveLeft grid) = grid
  | otherwise = chooseGrid (chooseGridIndex utilities maximumm 0) (grids) 
    where utilities = runEval (parUtility (grids) a b maxDepth)
          maximumm = maxU utilities 
          grids = getChildren grid


parUtility :: [Grid] -> Integer -> Integer -> Int -> Eval [Integer] 
parUtility grids a b maxDepth = parList rpar [chance c a b (maxDepth - 1) | c <- grids]

maxU :: [Integer] -> Integer
maxU utilities = maximum utilities 

chooseGridIndex :: [Integer] -> Integer -> Int -> Int 
chooseGridIndex [] _ _ = error "wrong number of elements"
chooseGridIndex (x:xs) maxUtil starter
                | x == maxUtil = starter
                | otherwise = chooseGridIndex xs maxUtil (starter + 1) 

chooseGrid :: Int -> [Grid] -> Grid     
chooseGrid index grids = grids !! index 

maximize :: Grid -> Integer -> Integer -> Int -> (Grid, Integer) 
maximize grid a b maxDepth
  | maxDepth == 0 || not (isMoveLeft grid) = (grid, utility grid)
  | otherwise = maxHelper (getChildren grid) grid a b (-999999999999) maxDepth

maxHelper :: [Grid] -> Grid -> Integer -> Integer -> Integer -> Int -> (Grid, Integer)
maxHelper [] maxChild _ _ maxUtility _ = (maxChild, maxUtility)
maxHelper (c:children) maxChild a b maxUtility maxDepth
  | util > maxUtility = maxHelper (c:children) c a b util maxDepth
  | maxUtility >= b = maxHelper children maxChild a b maxUtility maxDepth
  | maxUtility > a = maxHelper (c:children) maxChild maxUtility b maxUtility maxDepth
  | otherwise = maxHelper children maxChild a b maxUtility maxDepth
    --where utility = runEval $ rpar (chance c a b (maxDepth - 1))
    where util = chance c a b (maxDepth - 1)

chance :: Grid -> Integer -> Integer -> Int -> Integer 
chance grid a b maxDepth  
  | maxDepth == 0 = utility grid
  | otherwise = toInteger (round ((9/10) * (realToFrac (minimize grid a b 2 (maxDepth - 1))))) + toInteger (round ((1/10) * (realToFrac (minimize grid a b 4 (maxDepth - 1)))))

minimize :: Grid -> Integer -> Integer -> Int -> Int -> Integer
minimize grid a b tileVal maxDepth
  | maxDepth == 0 || availableCells grid == 0 = utility grid
  | otherwise = minHelper grid (getZeroes grid) tileVal a b 999999999999 maxDepth

minHelper :: Grid -> [(Int, Int)] -> Int -> Integer -> Integer -> Integer -> Int -> Integer
minHelper _ [] _ _ _ minUtility _ = minUtility
minHelper grid (c:cells) tVal a b minUtility maxDepth
  | util < minUtility = minHelper grid (c:cells) tVal a b util maxDepth
  | minUtility <= a = minHelper grid cells tVal a b minUtility maxDepth
  | minUtility < b = minHelper grid (c:cells) tVal a minUtility minUtility maxDepth
  | otherwise = minHelper grid cells tVal a b minUtility maxDepth
    where (_, util) = maximize (setSquare grid c tVal) a b (maxDepth - 1)


maxTile :: Grid -> Int
maxTile b = maximum $ map maximum b


gameLoop :: Grid -> Int -> IO ()
gameLoop grid movess
    | isMoveLeft grid = do
        printGrid grid
        if check2048 grid
        then print movess
        else do --new_grid <- newGrid grid
                let newGrid = parMaximize grid (-999999999999) 999999999999 6
                if grid /= newGrid
                then do new <- addTile newGrid
                        gameLoop new (movess+1)
                else gameLoop grid (movess+1)
    | otherwise = do
        printGrid grid
        putStrLn "Game over"
        print movess

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    grid <- start
    gameLoop grid 0
