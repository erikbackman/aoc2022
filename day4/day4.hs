{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import qualified Data.Text as T
import Control.Arrow

main :: IO ()
main = interact (show . (part1 &&& part2))

parseRanges :: String -> [[(Int, Int)]]
parseRanges inp = fmap range . T.splitOn "," . T.pack <$> lines inp
    where range txt = let [a,b] = T.splitOn "-" txt 
                      in (read (T.unpack a), read (T.unpack b))

solveWith overlapCheck = length . filter overlapCheck . parseRanges

part1 :: String -> Int
part1 = solveWith $ \[(a1,b1), (a2,b2)] -> a1 <= a2 && b1 >= b2 || a2 <= a1 && b2 >= b1

part2 :: String -> Int
part2 = solveWith $ \[r1,r2] -> (not . null) $ fromRange r1 `intersect` fromRange r2
    where fromRange (a,b) = [a .. b]
