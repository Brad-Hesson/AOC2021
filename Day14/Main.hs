module Day14.Main where

import Common (splitBy)
import Control.Arrow (Arrow ((&&&), (***)))
import Control.Monad ((<=<))
import Data.List (tails)
import qualified Data.Map as M

type PairMap = M.Map String Int

type BondMap = M.Map String PairMap

type CharMap = M.Map Char Int

templateFromFile :: FilePath -> IO String
templateFromFile = return . head . lines <=< readFile

pairmapFromTemplate :: String -> PairMap
pairmapFromTemplate = M.fromList . flip zip (repeat 1) . map (take 2) . init . init . tails

readPair :: String -> BondMap
readPair =
  let f [[a, b], [c]] = ([a, b], M.unionsWith (+) [M.singleton [a, c] 1, M.singleton [c, b] 1])
      f _ = undefined
   in uncurry M.singleton . f . splitBy (`elem` " ->")

bondmapFromFile :: FilePath -> IO BondMap
bondmapFromFile = return . M.unions . map readPair . drop 2 . lines <=< readFile

step :: BondMap -> PairMap -> PairMap
step bm = M.unionsWith (+) . map (uncurry (flip M.map) . ((bm M.!) *** (*))) . M.assocs

charmapFromPairmap :: String -> PairMap -> CharMap
charmapFromPairmap s =
  let f ([a, b], n) = M.unionsWith (+) [M.singleton a n, M.singleton b n]
      f _ = undefined
      ends = M.unionsWith (+) [M.singleton (head s) 1, M.singleton (last s) 1]
   in M.map (`div` 2) . M.unionsWith (+) . (ends :) . map f . M.assocs

part1 :: BondMap -> String -> Int
part1 bm tmp = uncurry (-) . (maximum &&& minimum) . M.elems . charmapFromPairmap tmp . (!! 10) . iterate (step bm) . pairmapFromTemplate $ tmp

part2 :: BondMap -> String -> Int
part2 bm tmp = uncurry (-) . (maximum &&& minimum) . M.elems . charmapFromPairmap tmp . (!! 40) . iterate (step bm) . pairmapFromTemplate $ tmp

main :: IO ()
main = do
  let fp = "Day14/input.txt"
  tmp <- templateFromFile fp
  bm <- bondmapFromFile fp
  putStr "Part 1: "
  print $ part1 bm tmp
  putStr "Part 2: "
  print $ part2 bm tmp