module Day15.Main where

import Control.Arrow (Arrow (second, (&&&), (***)))
import Data.Array (Array, array, assocs, bounds, listArray, (!))
import Data.Char (digitToInt, isNumber)
import Data.List (sortOn)
import qualified Data.Map as M

type Maze = M.Map Point Node

type Point = (Int, Int)

data Node = Node {isVisited :: Bool, getCost :: Int} deriving (Show)

arrayFromFile :: FilePath -> IO (Array (Int, Int) Int)
arrayFromFile fp = do
  raw <- readFile fp
  let rs = length . lines $ raw
  let cs = length . head . lines $ raw
  return . listArray ((0, 0), (rs - 1, cs - 1)) . map digitToInt . filter isNumber $ raw

mazeFromArray :: Array (Int, Int) Int -> Maze
mazeFromArray = M.fromList . map (second $ Node False) . assocs

adjacents :: Maze -> Point -> [Point]
adjacents mz =
  let offsets (xi, yi) = [(x + xi, y + yi) | (x, y) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]]
   in filter (`M.member` mz) . offsets

setVisited :: Point -> Maze -> Maze
setVisited p mz =
  let setVisitedOne (Node _ n) = Node True n
   in M.adjust setVisitedOne p mz

dijkstra :: Point -> Maze -> [(Point, Int)] -> [(Point, Int)]
dijkstra endP mz (pqh : pqrest)
  | endP == fst pqh = [pqh]
  | isVisited . (mz M.!) . fst $ pqh = dijkstra endP mz pqrest
  | all (isVisited . (mz M.!)) . adjacents mz . fst $ pqh = dijkstra endP mz pqrest
  | otherwise =
    let mz' = setVisited (fst pqh) mz
        adjs = adjacents mz . fst $ pqh
        fltadjs = filter (not . all (isVisited . (mz' M.!)) . adjacents mz) . filter (not . isVisited . (mz' M.!)) $ adjs
        pqadjs = map (id &&& (+ snd pqh) . getCost . (mz M.!)) fltadjs
        pq' = sortOn snd $ pqadjs ++ pqrest
     in pqh : dijkstra endP mz' pq'

part1 :: Array (Int, Int) Int -> Int
part1 arr = snd . last . dijkstra (snd . bounds $ arr) (mazeFromArray arr) $ [((0, 0), 0)]

part2 :: Array (Int, Int) Int -> Int
part2 arr =
  let (sx, sy) = ((+ 1) *** (+ 1)) . snd . bounds $ arr
      f x y = (\x -> if x > 9 then x - 9 else x) . (+ (y `div` sy)) . (+ (x `div` sx)) . (arr !) $ (x `mod` sx, y `mod` sy)
      lst = [((x, y), f x y) | x <- [0 .. sx * 5 - 1], y <- [0 .. sy * 5 - 1]]
      mz = mazeFromArray . array ((0, 0), (sx * 5 - 1, sy * 5 - 1)) $ lst
      endPoint = (sx * 5 - 1, sy * 5 - 1)
      soln = dijkstra endPoint mz [((0, 0), 0)]
   in snd . last $ soln

main :: IO ()
main = do
  arr <- arrayFromFile "Day15/sample.txt"
  putStr "Part 1: "
  print $ part1 arr
  putStr "Part 2: "
  print $ part2 arr