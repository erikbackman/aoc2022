module Day5 where

import Data.List
import Data.Char
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Text.Read (readMaybe)

part1 = solve (move reverse)
part2 = solve (move id)

solve :: Mover -> String -> Maybe String
solve mover str = applyMoves <$> parse str
  where applyMoves (crates,moves) = topCrates $ foldl mover crates moves
        topCrates = fmap head . M.elems

type Mover = (IntMap [Char] -> Move -> IntMap [Char])
data Move = Move { count :: Int, from :: Int, to :: Int } deriving Show

parse :: String -> Maybe (IntMap [Char], [Move])
parse str = do
  (crateLines, instructions) <- splitInput (lines str)
  moves <- traverse parseMove instructions
  pure (mkStacks crateLines, moves)
      where
        mkStacks xs = M.fromList (zip [1..] (parseCrates xs))
        splitInput txt = fmap (flip splitAt txt) (findIndex (isPrefixOf "move") txt)
        parseCrates inp = filter (not . null) $ fmap (filter (isAlpha)) (transpose inp)
        parseMove str = let [a,b,c] = filter (all isNumber) (words str) in 
                        Move <$> readMaybe a <*> readMaybe b <*> readMaybe c 


popn :: Int -> [a] -> Maybe ([a], [a])
popn n xs = if length xs >= n then Just (splitAt n xs) else Nothing
     
popnAt :: Int -> Int -> IntMap [Char] -> Maybe ([Char], IntMap [Char])
popnAt n i m = do
  chars <- M.lookup i m
  (popped, remain) <- popn n chars
  pure (popped, M.adjust (const remain) i m)  
  
pushTo :: [Char] -> Int -> IntMap [Char] -> IntMap [Char]
pushTo cs i m = M.adjust (\s -> cs <> s) i m

move :: ([Char] -> [Char]) -> IntMap [Char] -> Move -> IntMap [Char]
move order m (Move cnt f t) = 
    maybe m (\(popd, upd) -> pushTo (order popd) t upd) (popnAt cnt f m)
