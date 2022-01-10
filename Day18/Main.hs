module Day18.Main where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad ((<=<))
import Text.ParserCombinators.ReadP (char)
import Text.ParserCombinators.ReadPrec (lift, readPrec_to_P)
import Text.Read (Read (readPrec))

data SFNum = Reg {getInt :: Integer} | Pair SFNum SFNum deriving (Eq, Ord)

instance Read SFNum where
  readPrec =
    let reg = Reg <$> readPrec_to_P readPrec 0
        pair = do
          _ <- char '['
          a <- sfnum
          _ <- char ','
          b <- sfnum
          _ <- char ']'
          return $ Pair a b
        sfnum = reg <|> pair
     in lift sfnum

explode :: Int -> SFNum -> Either SFNum (SFNum, (SFNum, SFNum))
explode _ sfn@(Reg _) = Left sfn
explode d (Pair a b)
  | d == 4 = Right (Reg 0, (a, b))
  | otherwise = case (explode (d + 1) a, explode (d + 1) b) of
    (Right (a', (l, r)), _) -> Right (Pair a' (r + b), (l, 0))
    (_, Right (b', (l, r))) -> Right (Pair (a + l) b', (0, r))
    _ -> Left (Pair a b)

split :: SFNum -> Either SFNum SFNum
split (Reg n)
  | n >= 10 = Right $ Pair (Reg . floor . (/ 2) . fromIntegral $ n) (Reg . ceiling . (/ 2) . fromIntegral $ n)
  | otherwise = Left $ Reg n
split (Pair a b) = case (split a, split b) of
  (Right a', _) -> Right $ Pair a' b
  (_, Right b') -> Right $ Pair a b'
  _ -> Left $ Pair a b

reduceSFNum :: SFNum -> SFNum
reduceSFNum sfn = case explode 0 sfn of
  Right (sfn', _) -> reduceSFNum sfn'
  Left _ -> case split sfn of
    Right sfn' -> reduceSFNum sfn'
    Left _ -> sfn

instance Num SFNum where
  (Reg a) + (Reg b) = Reg $ a + b
  a@(Reg _) + (Pair l r) = Pair (a + l) r
  (Pair l r) + a@(Reg _) = Pair l (r + a)
  a + b = reduceSFNum $ Pair a b
  (Reg a) * (Reg b) = Reg $ a * b
  abs n@(Reg _) = n
  abs (Pair a b) = 3 * abs a + 2 * abs b
  fromInteger = Reg
  signum = error "Unreachable `signum`"
  negate = error "Unreachable `negate`"

sfnumsFromFile :: FilePath -> IO [SFNum]
sfnumsFromFile = return . map read . lines <=< readFile

part1 :: [SFNum] -> Integer
part1 = getInt . abs . sum

part2 :: [SFNum] -> Integer
part2 sfns = getInt $ maximum [abs $ a + b | a <- sfns, b <- sfns]

main :: IO ()
main = do
  sfns <- sfnumsFromFile "Day18/input.txt"
  putStr "Part 1: "
  print $ part1 sfns
  putStr "Part 2: "
  print $ part2 sfns