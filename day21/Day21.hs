module Day21 where

import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.String
import Data.Map.Strict (Map)
import Data.Char

data Expr = Normal Int | BinOp (Int -> Int -> Int) String String

pline :: Parser (String, Expr)
pline = do
  id <- manyTill anyChar (char ':')
  space
  expr <- num <|> op
  pure (id, expr)
  where
    word = many1 (satisfy isAlpha)
    num = Normal <$> read <$> many1 (satisfy isNumber)

    op = do
          rh <- word
          space
          op <- oneOf ['+', '-', '*', '/']
          space
          lh <- word

          pure $ BinOp (case op of
                          '+' -> (+) 
                          '-' -> (-) 
                          '*' -> (*) 
                          '/' -> div)
                 rh lh

plines :: Parser (Map String Expr)
plines = M.fromList <$> pline `sepEndBy` newline

parse' = parse plines ""

eval :: String -> Map String Expr -> Int
eval k m = go m node
    where
      go m' (Normal v) = v
      go m' (BinOp op lh rh) = op (eval lh m') (eval rh m')

      Just node = M.lookup k m

part1 inp = eval "root" <$> parse' inp 
