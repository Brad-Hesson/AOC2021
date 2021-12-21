module Day11.Main where

import Common (groupOn)
import Control.Arrow (Arrow (second))
import Data.Array
  ( Array,
    Ix (inRange),
    assocs,
    bounds,
    elems,
    indices,
    listArray,
    (!),
    (//),
  )
import Data.Char (digitToInt, isNumber)
import Data.List (partition)

type RC = (Int, Int)

type FlashMap = Array RC Int

flashmapFromFile :: FilePath -> IO FlashMap
flashmapFromFile fp = do
  raw <- readFile fp
  let cs = length . head . lines $ raw
  let rs = length . lines $ raw
  let arr = map digitToInt . fst . partition isNumber $ raw
  return $ listArray ((1, 1), (rs, cs)) arr

adjacents :: FlashMap -> RC -> [RC]
adjacents fm =
  let offsets (xi, yi) = [(x + xi, y + yi) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]
   in filter (inRange $ bounds fm) . offsets

incRC :: FlashMap -> RC -> FlashMap
incRC fm rc =
  let val = 1 + (fm ! rc)
      fm' = fm // [(rc, val)]
   in if val == 10 then foldl incRC fm' (fm' `adjacents` rc) else fm'

step :: FlashMap -> FlashMap
step fm =
  let fm' = foldl incRC fm . indices $ fm
   in (fm' //) . map (second $ const 0) . filter ((> 9) . snd) . assocs $ fm'

part1 :: FlashMap -> Int
part1 = length . filter (== 0) . concatMap elems . take 100 . drop 1 . iterate step

part2 :: FlashMap -> Int
part2 = fst . head . dropWhile (any (/= 0) . elems . snd) . zip [0 ..] . iterate step

main :: IO ()
main = do
  fm <- flashmapFromFile "Day11/input.txt"
  putStr "Part 1: "
  print $ part1 fm
  putStr "Part 2: "
  print $ part2 fm