module Day22.Main where

import Common
import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

data Range = Range Int Int deriving (Show, Eq, Ord)

data Cube = Cube {getXRange :: Range, getYRange :: Range, getZRange :: Range} deriving (Show, Eq, Ord)

data RBStep = RBStep {getToggle :: Bool, getCube :: Cube} deriving (Show, Eq, Ord)

newtype Region = Region {getCubes :: [Cube]} deriving (Show, Eq, Ord)

parseRBStep :: String -> RBStep
parseRBStep s =
  let [onoff, xt, yt, zt] = splitBy (`elem` " ,") s
      toggle = onoff == "on"
      parseRange = uncurry Range . (head &&& (+ 1) . last) . sort . map read . filter (isNumber . last) . groupOn (`elem` "=.xyz")
   in RBStep {getToggle = toggle, getCube = Cube {getXRange = parseRange xt, getYRange = parseRange yt, getZRange = parseRange zt}}

rbStepsFromFile :: FilePath -> IO [RBStep]
rbStepsFromFile = return . map parseRBStep . lines <=< readFile

rangeIntersection :: Range -> Range -> Maybe Range
rangeIntersection (Range min1 max1) (Range min2 max2) =
  if max1 < min2 || max2 < min1
    then Nothing
    else Just $ Range (max min1 min2) (min max1 max2)

cubeIntersection :: Cube -> Cube -> Maybe Cube
cubeIntersection (Cube xr1 yr1 zr1) (Cube xr2 yr2 zr2) =
  case mapMaybe (uncurry rangeIntersection) [(xr1, xr2), (yr1, yr2), (zr1, zr2)] of
    [xr, yr, zr] -> Just $ Cube xr yr zr
    _ -> Nothing

getVolume :: Cube -> Int
getVolume Cube {getXRange = Range xmin xmax, getYRange = Range ymin ymax, getZRange = Range zmin zmax} = (xmax - xmin) * (ymax - ymin) * (zmax - zmin)

trimRBStep :: Cube -> RBStep -> Maybe RBStep
trimRBStep trim rbs = case cubeIntersection trim . getCube $ rbs of
  Just cube -> Just $ rbs {getCube = cube}
  Nothing -> Nothing

splitRange :: Int -> Range -> [Range]
splitRange n range@(Range rmin rmax) = if rmin < n && n < rmax then [Range rmin n, Range n rmax] else [range]

splitCubeX :: Int -> Cube -> [Cube]
splitCubeX x cube@Cube {getXRange = xr} = map (\xr' -> cube {getXRange = xr'}) $ splitRange x xr

splitCubeY :: Int -> Cube -> [Cube]
splitCubeY y cube@Cube {getYRange = yr} = map (\yr' -> cube {getYRange = yr'}) $ splitRange y yr

splitCubeZ :: Int -> Cube -> [Cube]
splitCubeZ z cube@Cube {getZRange = zr} = map (\zr' -> cube {getZRange = zr'}) $ splitRange z zr

rangeSplitter :: Range -> Range -> [Int]
rangeSplitter (Range rl1 rh1) (Range rl2 rh2) = case (rl1 == rl2, rh1 == rh2) of
  (False, False) -> [max rl1 rl2, min rh1 rh2]
  (False, True) -> [max rl1 rl2]
  (True, False) -> [min rh1 rh2]
  (True, True) -> []

subtractCube :: Cube -> Cube -> [Cube]
subtractCube cube@(Cube rx ry rz) cutter = case cubeIntersection cube cutter of
  Nothing -> [cube]
  Just interCube@(Cube irx iry irz) ->
    let cuts = concat . zipWith map [splitCubeX, splitCubeY, splitCubeZ] . map (uncurry rangeSplitter) $ [(irx, rx), (iry, ry), (irz, rz)]
     in filter (/= interCube) $ foldr concatMap [cube] cuts

applyRBStep :: Region -> RBStep -> Region
applyRBStep (Region cubes) RBStep {getToggle = toggleOn, getCube = newCube} =
  if toggleOn
    then Region $ cubes ++ foldl (\ncs c -> concatMap (`subtractCube` c) ncs) [newCube] cubes
    else Region $ concatMap (`subtractCube` newCube) cubes

part1 :: [RBStep] -> Int
part1 rbs =
  let initRange = Range (-50) 50
      initCube = Cube initRange initRange initRange
   in sum . map getVolume . getCubes . foldl applyRBStep (Region []) . mapMaybe (trimRBStep initCube) $ rbs

part2 :: [RBStep] -> Int
part2 = sum . map getVolume . getCubes . foldl applyRBStep (Region [])

main :: IO ()
main = do
  rbs <- rbStepsFromFile "Day22/sample.txt"
  putStr "Part 1: "
  print $ part1 rbs
  putStr "Part 2: "
  print $ part2 rbs