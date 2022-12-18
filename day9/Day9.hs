module Day9 where

import qualified Linear as L
import Linear (V2(..), zero, (*^))
import Data.List
import qualified Data.Set as S
                
direction :: Floating a => String -> V2 a
direction "U" = V2   0   1
direction "D" = V2   0 (-1)
direction "L" = V2 (-1)  0
direction "R" = V2   1   0
direction  _  = error "Invalid dir"

start = V2 0.0 0.0

linesToMotions :: String -> [V2 Double]
linesToMotions s = let dirVectors = fmap (parse . words) $ lines s
                   in snd $ foldl genfield (start, []) dirVectors 
  where
    parse :: [String] -> (Int, V2 Double)
    parse [dir, n] = (read n, direction dir)

    genfield (p0,acc) (n,v)  =
        let end  = p0 + (fromIntegral n)*^v
            hvecs = take n $ iterate (+ v) (p0 + v)
        in (end, acc <> hvecs)

part1 :: String -> Int
part1 = length . fst . foldl f (S.insert start S.empty, start) . linesToMotions
    where
      f (s, t) (h@(V2 hx hy)) =
          let r@(V2 dx dy) = h - t in
          if abs dx > 1 || abs dy > 1 then 
              let t' = (t + (fmap signum r)) in (t' `S.insert` s, t')
          else (s,t)
