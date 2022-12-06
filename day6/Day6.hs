module Day6 where

import Data.List
import qualified Data.Set as S

solve :: Int -> String -> Maybe Int
solve len inp = (+len) <$> findIndex ((== len) . length . S.fromList) (windows len inp)
    where windows n = foldr (zipWith (:)) (repeat []) . take n . tails
