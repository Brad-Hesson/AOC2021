module Day6.Main where

import Data.Char
import Data.List
import Control.Arrow
import Control.Monad

type FishGen = [Int]

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = filter (not . f . head) . groupBy (curry $ uncurry (==) . (f***f))

fishFromFile :: FilePath -> IO FishGen
fishFromFile = return . map (subtract 1 . length) . group . sort . (++) [0..8] . map read . splitBy (==',') <=< readFile

rotateLeft1 :: [a] -> [a]
rotateLeft1 = liftM2 (++) tail (singleton . head)

nextGen :: FishGen -> FishGen
nextGen = uncurry (++) . second (\[a6,a7,a8]-> [a6+a8,a7,a8]) . splitAt 6 . rotateLeft1

genPop :: Int -> FishGen -> Int
genPop n = sum . head . drop n . iterate nextGen

part1 :: FishGen -> Int
part1 = genPop 80

part2 :: FishGen -> Int
part2 = genPop 256

main = do
  fish <- fishFromFile "Day6/input.txt"
  putStr "Part 1: "
  print . part1 $ fish
  putStr "Part 2: "
  print . part2 $ fish