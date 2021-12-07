module Day1.Main where

import Control.Arrow
import Control.Monad

depthsFromFile :: FilePath -> IO [Int]
depthsFromFile = return . map read . lines <=< readFile

movingSum :: Int -> [Int] -> [Int]
movingSum n l
  | n > length l = []
  | otherwise = sum (take n l) : movingSum n (tail l)

main :: IO ()
main = do
  depths <- depthsFromFile "Day1/input.txt"
  putStr "Part 1: "
  print . sum . map fromEnum . uncurry (zipWith (>)) . (drop 1 &&& id) $ depths
  putStr "Part 2: "
  print . sum . map fromEnum . uncurry (zipWith (>)) . (drop 1 &&& id) $ movingSum 3 depths