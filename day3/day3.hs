module Main where

prio :: Char -> Int
prio c | n >= 97 && n <= 122 = (n - fromEnum 'a') + 1
       | n >= 65 && n <= 90 = (n - fromEnum 'A') + 27
       | otherwise = 0
       where n = fromEnum c

dupes :: String -> String         
dupes s = 
    let (c1,c2) = splitAt ((length s)`div`2) s 
    in do
      x <- c1
      y <- c2
      if (x == y) then pure x else ""

both :: [String] -> String
both xs = fmap head $ fmap dupes xs

part1 :: String -> Int
part1 = sum . fmap prio . both . lines

windowOf :: Int -> [a] -> [[a]]
windowOf n xs
    | len == n = [xs]
    | len > n = let (w,rest) = splitAt n xs in w : windowOf n rest
    where len = length xs

threeCommon :: [String] -> String
threeCommon [x,y,z] = do
  xk <- x
  yk <- y
  zk <- z
  if (xk == yk && yk == zk) then pure xk else mempty

part2 :: String -> Int
part2 = sum . fmap (prio . head . threeCommon) . windowOf 3 . lines

main = interact (\inp -> show (part1 inp, part2 inp))
