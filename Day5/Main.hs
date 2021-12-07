{-# LANGUAGE TupleSections #-}
module Day5.Main where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map.Strict as M

data Line = Line {x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int}
  deriving (Show)

type TrailMap = M.Map (Int, Int) Int

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = filter (not . f . head) . groupBy (curry $ uncurry (==) . (f *** f))

parseLine :: String -> Line
parseLine s = Line x1 y1 x2 y2 where [x1, y1, x2, y2] = map read . splitBy (`elem` ", ->") $ s

linesFromFile :: FilePath -> IO [Line]
linesFromFile = return . map parseLine . lines <=< readFile

lineToTrailMap :: Line -> TrailMap
lineToTrailMap (Line x1 y1 x2 y2) = M.fromList . map (,1) $ coords
  where
    coords
      | x1 == x2 = [(x1, y) | y <- ys]
      | y1 == y2 = [(x, y1) | x <- xs]
      | otherwise = zip xs ys
    xs = [x1, (x1 + signum (x2 - x1)) .. x2]
    ys = [y1, (y1 + signum (y2 - y1)) .. y2]

part1 :: [Line] -> Int
part1 = length . M.filter (> 1) . M.unionsWith (+) . map lineToTrailMap . filter (\(Line x1 y1 x2 y2) -> x1 == x2 || y1 == y2)

part2 :: [Line] -> Int
part2 = length . M.filter (> 1) . M.unionsWith (+) . map lineToTrailMap

main = do
  ls <- linesFromFile "Day5/input.txt"
  putStr "Part 1: "
  print . part1 $ ls
  putStr "Part 2: "
  print . part2 $ ls