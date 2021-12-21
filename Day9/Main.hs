module Day9.Main where

import Common (iterMaybe, mapF)
import Control.Arrow (Arrow ((&&&)))
import Data.Array
  ( Array,
    Ix (inRange, range),
    bounds,
    listArray,
    (!),
  )
import Data.Char (digitToInt, isNumber)
import Data.List (group, partition, sort, sortOn)
import Data.Maybe (isNothing, listToMaybe)

type RC = (Int, Int)

type HeightMap = Array RC Int

heightmapFromFile :: FilePath -> IO HeightMap
heightmapFromFile fp = do
  raw <- readFile fp
  let cs = length . head . lines $ raw
  let rs = length . lines $ raw
  let arr = map digitToInt . fst . partition isNumber $ raw
  return $ listArray ((1, 1), (rs, cs)) arr

adjacents :: HeightMap -> RC -> [RC]
adjacents hm =
  let offsets (xi, yi) = [(x + xi, y + yi) | (x, y) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]]
   in filter (inRange $ bounds hm) . offsets

nextLowest :: HeightMap -> RC -> Maybe RC
nextLowest hm rc = fmap snd . listToMaybe . sortOn fst . filter ((hm ! rc >=) . fst) . map ((!) hm &&& id) . adjacents hm $ rc

part1 :: HeightMap -> Int
part1 hm =
  sum
    . map ((+ 1) . (hm !))
    . filter (isNothing . nextLowest hm)
    . range
    . bounds
    $ hm

part2 :: HeightMap -> Int
part2 hm =
  product
    . take 3
    . sortOn negate
    . map length
    . group
    . sort
    . map (iterMaybe $ nextLowest hm)
    . filter ((< 9) . (hm !))
    . range
    . bounds
    $ hm

main :: IO ()
main = do
  hm <- heightmapFromFile "Day9/input.txt"
  putStr "Part 1: "
  print $ part1 hm
  putStr "Part 2: "
  print $ part2 hm