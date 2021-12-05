module Day2.Main where

import Data.Char
import Data.List
import Control.Arrow
import Control.Monad

data Move = F Int | D Int | U Int
  deriving (Show)

movesFromFile :: FilePath -> IO [Move]
movesFromFile = return . map (f . partition isNumber) . lines <=< readFile
  where
    f (n,"forward ") = F $ read n
    f (n,"up ") = U $ read n
    f (n,"down ") = D $ read n

part1 (h,v) (F n) = (h+n,v)
part1 (h,v) (U n) = (h,v-n)
part1 (h,v) (D n) = (h,v+n)

part2 (h,v,a) (F n) = (h+n, v+n*a, a)
part2 (h,v,a) (U n) = (h, v, a-n)
part2 (h,v,a) (D n) = (h, v, a+n)

main :: IO ()
main = do
  moves <- movesFromFile "Day2/input.txt"
  putStr "Part 1: "
  print . (\(h,v) -> h*v) $ foldl part1 (0,0) moves
  putStr "Part 2: "
  print . (\(h,v,a) -> h*v) $ foldl part2 (0,0,0) moves