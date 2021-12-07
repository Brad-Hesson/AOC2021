module Day7.Main where

import Control.Arrow
import Control.Monad
import Data.List

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = filter (not . f . head) . groupBy (curry $ uncurry (==) . (f *** f))

positionsFromFile :: FilePath -> IO [Int]
positionsFromFile = return . map read . splitBy (== ',') <=< readFile

layoutPositions :: [Int] -> [Int]
layoutPositions = map (subtract 1 . length) . group . sort . uncurry (++) . ((\(a, b) -> [a .. b]) . (minimum &&& maximum) &&& id)

rightwardCost :: [Int] -> [Int]
rightwardCost = init . scanl (+) 0 . scanl1 (+)

rightwardCost2 :: [Int] -> [Int]
rightwardCost2 = scanl1 (+) . init . scanl (+) 0 . scanl1 (+)

biDirCostWith :: ([Int] -> [Int]) -> [Int] -> [Int]
biDirCostWith f = uncurry (zipWith (+)) . (reverse . f . reverse &&& f)

part1 :: [Int] -> Int
part1 = minimum . biDirCostWith rightwardCost . layoutPositions

part2 :: [Int] -> Int
part2 = minimum . biDirCostWith rightwardCost2 . layoutPositions

main = do
  pos_s <- positionsFromFile "Day7/input.txt"
  putStr "Part 1: "
  print $ part1 pos_s
  putStr "Part 2: "
  print $ part2 pos_s