module Day9.Main where

import Common (iterMaybe, mapF)
import Control.Arrow (Arrow ((&&&)))
import qualified Data.Array as A
import Data.Char (digitToInt, isNumber)
import Data.List (group, partition, sort, sortOn)
import Data.Maybe (isNothing, listToMaybe)

data HeightMap = HeightMap (A.Array Int Int) Int Int

type XY = (Int, Int)

idxHM :: HeightMap -> XY -> Int
idxHM (HeightMap arr w h) (x, y) = arr A.! (x + w * y)

heightmapFromFile :: FilePath -> IO HeightMap
heightmapFromFile fp = do
  raw <- readFile fp
  let w = length . head . lines $ raw
  let h = length . lines $ raw
  let arr = map digitToInt . fst . partition isNumber $ raw
  return $ HeightMap (A.listArray (0, length arr - 1) arr) w h

adjacents :: HeightMap -> XY -> [XY]
adjacents (HeightMap _ w h) (x, y) = filter (and . mapF preds) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    preds = [(>= 0) . fst, (>= 0) . snd, (< w) . fst, (< h) . snd]

nextLowest :: HeightMap -> XY -> Maybe XY
nextLowest hm xy = fmap snd . listToMaybe . sortOn fst . filter ((idxHM hm xy >=) . fst) . map (idxHM hm &&& id) . adjacents hm $ xy

part1 :: HeightMap -> Int
part1 hm@(HeightMap _ w h) =
  sum
    . map ((+ 1) . idxHM hm)
    . filter (isNothing . nextLowest hm)
    $ [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

part2 :: HeightMap -> Int
part2 hm@(HeightMap _ w h) =
  product
    . take 3
    . sortOn negate
    . map length
    . group
    . sort
    . map (iterMaybe $ nextLowest hm)
    . filter ((< 9) . idxHM hm)
    $ [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

main :: IO ()
main = do
  hm <- heightmapFromFile "Day9/input.txt"
  putStr "Part 1: "
  print $ part1 hm
  putStr "Part 2: "
  print $ part2 hm