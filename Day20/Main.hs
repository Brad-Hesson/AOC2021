{-# LANGUAGE TupleSections #-}

module Day20.Main where

import Common
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Maybe
import qualified Data.Set as S

type Point = (Int, Int)

data Image = Image Bool (S.Set Point)

numLit :: Image -> Int
numLit (Image _ imset) = length imset

type Key = [Bool]

dataFromFile :: FilePath -> IO (Key, Image)
dataFromFile =
  return
    . second
      ( Image False
          . S.fromList
          . concat
          . zipWith (\y xs -> map (,y) xs) [0 ..]
          . map (map fst . filter ((== '#') . snd) . zip [0 ..])
      )
    . (map (== '#') . head . head &&& head . tail)
    . splitBy (== "")
    . lines
    <=< readFile

compareXs :: Point -> Point -> Ordering
compareXs = curry $ uncurry compare . (fst *** fst)

compareYs :: Point -> Point -> Ordering
compareYs = curry $ uncurry compare . (snd *** snd)

checkpointsFromImage :: Image -> [Point]
checkpointsFromImage (Image _ imset) =
  let xl = fst . minimumBy compareXs $ imset
      xh = fst . maximumBy compareXs $ imset
      yl = snd . minimumBy compareYs $ imset
      yh = snd . maximumBy compareYs $ imset
   in [(x, y) | x <- [xl - 1 .. xh + 1], y <- [yl - 1 .. yh + 1]]

decFromBin :: [Bool] -> Int
decFromBin = sum . zipWith (*) (map (2 ^) [0 ..]) . map fromEnum . reverse

indexImage :: Point -> Image -> Bool
indexImage p (Image bkg imset) = bkg /= p `S.member` imset

checkPoint :: Key -> Image -> Point -> Bool
checkPoint key img (x, y) = (key !!) $ decFromBin [(x + dx, y + dy) `indexImage` img | dy <- [-1 .. 1], dx <- [-1 .. 1]]

stepImage :: Key -> Image -> Image
stepImage key img@(Image bkg _) =
  let invert = head key
      bkg' = invert && not bkg
      mapfunc p = if checkPoint key img p /= bkg' then Just p else Nothing
   in Image bkg' . S.fromList . mapMaybe mapfunc . checkpointsFromImage $ img

part1 :: Key -> Image -> Int
part1 key = numLit . (!! 2) . iterate (stepImage key)

part2 :: Key -> Image -> Int
part2 key = numLit . (!! 50) . iterate (stepImage key)

main :: IO ()
main = do
  (key, img) <- dataFromFile "Day20/sample.txt"
  putStr "Part 1: "
  print $ part1 key img
  putStr "Part 2: "
  print $ part2 key img