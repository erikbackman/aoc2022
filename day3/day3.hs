module Main where

import Data.List
import Data.Monoid
import Control.Monad
import Control.Arrow

prio :: Char -> Int
prio c | n >= 97 && n <= 122 = (n - fromEnum 'a') + 1
       | n >= 65 && n <= 90 = (n - fromEnum 'A') + 27
       | otherwise = 0
       where n = fromEnum c

part1 :: String -> Int
part1 s = getSum . foldMap (Sum . prio) $ (nub . dupes) =<< lines s
          where dupes s = let (c1,c2) = splitAt ((length s) `div` 2) s 
                          in intersect c1 c2

part2 :: String -> Int
part2 = getSum . foldMap (Sum . prio . head . common) . groupsOf 3 . lines
        where common = foldr1 intersect
              groupsOf n = takeWhile (not . null) . unfoldr (Just . splitAt n)

main = interact (show . (part1 &&& part2))
