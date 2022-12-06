module Day6 where

import Data.List
import qualified Data.Set as S
import Control.Arrow ((&&&))

findIx :: Int -> String -> Maybe Int
findIx len inp = (+len) <$> findIndex ((== len) . length . S.fromList) (windows len inp)
    where windows n = foldr (zipWith (:)) (repeat []) . take n . tails

day6 = (findIx 4 &&& findIx 14)

