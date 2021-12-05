module Day3.Main where

import Control.Arrow
import Control.Monad
import Data.List

type Bin = [Bool]

rowsFromFile :: FilePath -> IO [Bin]
rowsFromFile = return . map (map (=='1')) . lines <=< readFile

binToDec :: Bin -> Int
binToDec = sum . zipWith (*) (map (2^) [0..]) . reverse . map fromEnum

commonality :: (Ord a, Eq a) => [a] -> [a]
commonality = map snd . sortOn fst . map (length &&& head) . group . sort

part1 :: [Bin] -> Int
part1 = uncurry (*) . (binToDec &&& binToDec . map not) . map (head . commonality) . transpose

part2 :: [Bin] -> Int
part2 = uncurry (*) . (solveBy most &&& solveBy least) where
  solveBy f = binToDec . head . head . dropWhile ((>1) . length) . flip (scanl (reduceBy f)) [0..]
  reduceBy f l i = filter ((!!i) . zipWith (==) (f l)) l
  least = map (head . commonality) . transpose
  most = map not . least

main = do
  rows <- rowsFromFile "Day3/input.txt"
  putStr "Part 1: "
  print . part1 $ rows
  putStr "Part 2: "
  print . part2 $ rows