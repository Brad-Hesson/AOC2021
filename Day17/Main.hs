module Day17.Main where

import Common ( splitBy )
import Control.Arrow ( Arrow((***), (&&&)) )
import Control.Monad ( (<=<) )
import Data.List ( nub, sort, sortOn )

boundsFromFile :: FilePath -> IO ([Int], [Int])
boundsFromFile =
  let f [xs, ys] = (xs, ys)
   in return . f . map (map read . splitBy (== '.')) . drop 2 . splitBy (`elem` ", xy=") <=< readFile

orderedBounds :: RealFloat i => [Int] -> [i]
orderedBounds = map fromIntegral . sortOn abs

vyMax :: [Int] -> Int
vyMax = round . subtract 0.5 . abs . (+ 0.5) . last . orderedBounds

part1 :: [Int] -> Int
part1 ybs = let vym = fromIntegral $ vyMax ybs in round $ (vym * vym + vym + 0.25) * 0.5

intsBetween :: RealFloat i => [i] -> [Int]
intsBetween bs = let [i1, i2] = sort bs in [(ceiling i1) .. floor i2]

ts :: RealFloat i => [Int] -> i -> [Int]
ts ybs vy =
  let tofy y = vy + 0.5 + sqrt (vy * vy + vy + 0.25 - 2 * fromIntegral y)
   in intsBetween $ map tofy ybs

vxs :: (Enum i, RealFloat i) => [Int] -> i -> [Int]
vxs xbs t =
  let xofv v = if t > v then 0.5 * (v * v + v + 0.25) else -0.5 * t * (t - 2 * v - 1)
      [b1, b2] = map abs . orderedBounds $ xbs
      sign = fromIntegral . signum . head $ xbs
   in map (round . (* sign) . fst) . takeWhile ((<= b2) . snd) . dropWhile ((< b1) . snd) . map (id &&& xofv) $ [1 ..]

part2 :: [Int] -> [Int] -> Int
part2 xbs ybs =
  let vym = vyMax ybs
      vysAndTs = filter (not . null . snd) . map (id &&& ts ybs . fromIntegral) $ [-vym - 1 .. vym]
   in length . nub . concatMap (uncurry zip . (repeat *** concatMap (vxs xbs . fromIntegral))) $ vysAndTs

main :: IO ()
main = do
  (xs, ys) <- boundsFromFile "Day17/input.txt"
  putStr "Part 1: "
  print $ part1 ys
  putStr "Part 2: "
  print $ part2 xs ys