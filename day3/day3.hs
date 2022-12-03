module Main where

import Data.List
import Data.Monoid
import Control.Monad

prio :: Char -> Int
prio c | n >= 97 && n <= 122 = (n - fromEnum 'a') + 1
       | n >= 65 && n <= 90 = (n - fromEnum 'A') + 27
       | otherwise = 0
       where n = fromEnum c

part1 :: String -> Int
part1 s = getSum . foldMap (Sum . prio) $ (nub . dupes) =<< lines s
          where dupes s = let (c1,c2) = splitAt ((length s) `div` 2) s 
                          in intersect c1 c2

windowOf :: Int -> [a] -> [[a]]
windowOf n xs
    | len == n   = [xs]
    | len > n   = let (w,rest) = splitAt n xs in w : windowOf n rest
    | otherwise = []
    where len = length xs

part2 :: String -> Int
part2 = getSum . foldMap (Sum . prio . head . common) . windowOf 3 . lines
        where common = foldr1 intersect

main = interact (\inp -> show (part1 inp, part2 inp))
