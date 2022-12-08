module Day8 where

import qualified Data.Matrix as M
import Data.Matrix (Matrix)

data Dir = N | E | W | S
         deriving (Eq, Show, Enum)

nextPos :: Dir -> (Int, Int) -> (Int, Int)
nextPos dir (x,y) = case dir of
                   N -> (x, y+1)
                   S -> (x,y-1)
                   W -> (x-1,y)
                   E -> (x+1,y)

getAt m (x,y) = M.safeGet y x m

-- given a point and a direction, try to look in the given direction
canSeeOutDir :: Matrix Char -> (Int, Int) -> Dir -> Bool
canSeeOutDir m start@(x,y) dir = all shorter inPath
    where
      shorter x = getAt m x < getAt m start
      inPath = takeWhile (not . null . getAt m) $ tail $ iterate (nextPos dir) start

-- given a point, try to look in every direction
canSeeOut :: (Int, Int) -> Matrix Char -> Bool
canSeeOut p m = any (canSeeOutDir m p) [N, S, E, W]

innerPoints :: Matrix a -> [(Int, Int)]
innerPoints m = [ (x,y) | x <- [2 .. M.ncols m - 1], y <- [2 .. M.nrows m - 1] ]

nvisible :: Matrix Char -> Int
nvisible m = 4*(M.nrows m) - 4 + foldr f 0 (innerPoints m)
             where f p s = if canSeeOut p m then s+1 else s

part1 :: String -> Int
part1 = nvisible . M.fromLists . lines

