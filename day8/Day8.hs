module Day8 where

import qualified Data.Matrix as M
import Data.Matrix (Matrix)
import Linear (V2(..))
import Data.Ord (comparing)

innerPoints m = [ V2 x y | x <- [2 .. M.ncols m - 1], y <- [2 .. M.nrows m - 1] ]
getAt m (V2 x y) = M.safeGet y x m 

-- from a starting point, look in a direction and return the ordering in that path
lookInDir :: Matrix Char -> V2 Int -> V2 Int -> [Ordering]
lookInDir m start dir = tail $ scanl f EQ path
    where f _ xy = comparing (getAt m) xy start
          path = takeWhile (not . null . getAt m) $ iterate (+dir) (start+dir)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive f = (\(a,b) -> a ++ foldr (const . pure) [] b) . span f

-- look in every direction and return their ordering.
look :: Matrix Char -> V2 Int -> [[Ordering]]
look m p = lookInDir m p <$> [i, -i, j, -j]
    where i = V2 1 0; j = V2 0 (-1)

parseForest = M.fromLists . lines

-- part1
canSeeOut :: Matrix Char -> V2 Int -> Bool
canSeeOut m p = any (all (== LT)) (look m p)

nvisible :: Matrix Char -> Int
nvisible m = 4*M.nrows m - 4 + foldr f 0 (innerPoints m)
    where f p s = s+fromEnum (canSeeOut m p)

part1 = nvisible . parseForest

--- part2
scenicScore :: Matrix Char -> V2 Int -> Int
scenicScore m p = product $ length . (takeWhileInclusive (== LT)) <$> look m p

part2 = maximum . (\m -> fmap (scenicScore m) (innerPoints m)) . parseForest
