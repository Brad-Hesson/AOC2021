module Day12.Main where

import Common (splitBy)
import Control.Arrow (Arrow (second, (&&&)))
import Data.Char (isUpper)
import qualified Data.Map as M
import qualified Data.Set as S

data Cave = Start | End | Big String | Small String deriving (Show, Eq, Ord)

type CaveMap = M.Map Cave (S.Set Cave)

type VisitedMap = M.Map Cave Int

type Trail = [Cave]

readCave :: String -> Cave
readCave "start" = Start
readCave "end" = End
readCave s = if isUpper . head $ s then Big s else Small s

canEnter1 :: VisitedMap -> Cave -> Bool
canEnter1 vm c@(Small _) = (vm M.! c) == 0
canEnter1 _ Start = False
canEnter1 _ _ = True

canEnter2 :: VisitedMap -> Cave -> Bool
canEnter2 vm c@(Small _) =
  let f (Small _) = True
      f _ = False
   in (vm M.! c) == 0 || (all ((< 2) . snd) . filter (f . fst) . M.assocs $ vm)
canEnter2 _ Start = False
canEnter2 _ _ = True

incVM :: Cave -> VisitedMap -> VisitedMap
incVM = M.alter $ fmap (+ 1)

cavemapFromFile :: FilePath -> IO CaveMap
cavemapFromFile path = do
  raw <- readFile path
  let rawPaths = map (map readCave . splitBy (== '-')) . lines $ raw
  let expandPaths [f, t] = [(f, t), (t, f)]
  return . M.unionsWith S.union . map (uncurry M.singleton . second S.singleton) . concatMap expandPaths $ rawPaths

visitedmapFromCavemap :: CaveMap -> VisitedMap
visitedmapFromCavemap = M.fromList . flip zip (repeat 0) . M.keys

trailsFromCavemapWith :: (VisitedMap -> Cave -> Bool) -> (CaveMap, VisitedMap) -> Cave -> [Trail]
trailsFromCavemapWith _ _ End = [[End]]
trailsFromCavemapWith f (cm, vm) c = map (c :) . concatMap (trailsFromCavemapWith f (cm, incVM c vm)) . filter (f (incVM c vm)) . S.toList $ (cm M.! c)

part1 :: CaveMap -> Int
part1 = length . flip (trailsFromCavemapWith canEnter1) Start . (id &&& visitedmapFromCavemap)

part2 :: CaveMap -> Int
part2 = length . flip (trailsFromCavemapWith canEnter2) Start . (id &&& visitedmapFromCavemap)

main :: IO ()
main = do
  cm <- cavemapFromFile "Day12/input.txt"
  putStr "Part 1: "
  print $ part1 cm
  putStr "Part 2: "
  print $ part2 cm