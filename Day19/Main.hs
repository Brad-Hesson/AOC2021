{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Day19.Main where

import Common
import Control.Arrow
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)

type Vec = [Int]

type Pos = Vec

type Dist = Vec

type Scan = [Pos]

type DistMap = M.Map Dist [Pos]

type Transform = Pos -> Pos

instance Eq Transform where
  f1 == f2 = f1 [1, 2, 3] == f2 [1, 2, 3]

instance Show Transform where
  show f = "[0,0,0] -> " ++ show (f [0, 0, 0])

coordsFromFile :: FilePath -> IO [Scan]
coordsFromFile =
  return
    . map (map read)
    . splitBy (isPrefixOf "[---")
    . map (\s -> "[" ++ s ++ "]")
    . filter (/= "")
    . lines
    <=< readFile

normDistFromVecs :: (Pos, Pos) -> Dist
normDistFromVecs (x, y) = sort . map abs $ zipWith subtract x y

distMapFromScan :: Scan -> DistMap
distMapFromScan = M.fromList . map (\(p1, p2) -> (normDistFromVecs (p1, p2), [p1, p2])) . pairs

pointSetFromDistMaps :: DistMap -> DistMap -> [[[Pos]]]
pointSetFromDistMaps a b = M.elems $ M.intersectionWith (\x y -> [x, y]) a b

transforms :: [Transform]
transforms =
  let num_perms = 6
      num_negs = 8
      negs n = map (\x -> n `div` (2 ^ x) `mod` 2 * 2 - 1) [0 .. 2]
      func n = zipWith (*) (negs (n `div` num_perms)) . (!! (n `mod` num_perms)) . permutations
   in map func [0 .. num_perms * num_negs - 1]

findTransforms :: [[Pos]] -> [Transform]
findTransforms [[a, b], [c, d]] =
  let tcPairs p = zip (cycle transforms) . map (zipWith (-) p) $ mapF transforms c ++ mapF transforms d
   in map (\(t, c) -> zipWith (+) c . t) $ intersectBy (\(_, c1) (_, c2) -> c1 == c2) (tcPairs a) (tcPairs b)
findTransforms _ = undefined

transformFromScans :: Scan -> Scan -> Maybe Transform
transformFromScans s1 s2 =
  let possibles = map findTransforms $ pointSetFromDistMaps (distMapFromScan s1) (distMapFromScan s2)
      collect l
        | length l >= 12 = listToMaybe . foldl1 intersect $ l
        | otherwise = Nothing
   in collect possibles

collapseScans :: [Scan] -> Scan
collapseScans (s1 : s2 : rest) =
  let transform = transformFromScans s1 s2
      f Nothing = collapseScans ((s1 : rest) ++ [s2])
      f (Just t) = collapseScans (combineWith t s1 s2 : rest)
      combineWith t s1 s2 = nub $ s1 ++ map t s2
   in f transform
collapseScans [s1] = s1
collapseScans _ = undefined

manhatDist :: Pos -> Pos -> Int
manhatDist p1 p2 = sum . map abs $ zipWith subtract p1 p2

part1 :: [Scan] -> Int
part1 = length . collapseScans

part2 :: [Scan] -> Int
part2 ss =
  let s_all = collapseScans ss
   in maximum . map (uncurry manhatDist) . pairs $ mapF (mapMaybe (transformFromScans s_all) ss) [0, 0, 0]

main :: IO ()
main = do
  coords <- coordsFromFile "Day19/sample.txt"
  putStr "Part 1: "
  print $ part1 coords
  putStr "Part 2: "
  print $ part2 coords