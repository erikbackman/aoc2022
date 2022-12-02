module Main where

import Data.Monoid

score :: String -> Sum Int
score [l,_,r] = let opponent = fromEnum r - fromEnum 'X'
                    choosen = (fromEnum l - fromEnum 'A' - 1 + opponent) `mod` 3 
                in Sum $ choosen+1 + 3*opponent
score _ = Sum 0

part2 :: String -> String
part2 = show . getSum . foldMap score . lines

main = interact part2
