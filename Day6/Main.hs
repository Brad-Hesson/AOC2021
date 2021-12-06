module Day6.Main where

import Data.Char
import Data.List
import Control.Arrow
import Control.Monad
import Debug.Trace

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = filter (not . f . head) . groupBy (curry $ uncurry (==) . (f***f))

fishFromFile :: FilePath -> IO [Int]
fishFromFile = return . map read . splitBy (==',') <=< readFile

nextGen :: [Int] -> [Int]
nextGen = map (\x-> if x == -1 then 6 else x)
  . liftM2 (++) (flip take (repeat 8) . sum . map (fromEnum . (-1==))) id
  . map (flip (-) 1)

part1 :: [Int] -> Int
part1 = length . head . drop 80 . iterate' nextGen

part2 :: [Int] -> Int
part2 = length . head . drop 100 . iterate' nextGen

main = do
  fish <- fishFromFile "Day6/input.txt"
  putStr "Part 1: "
  print . part1 $ fish
  putStr "Part 2: "
  print . part2 $ fish