module Day13.Main where

import Common (groupOn, splitBy)
import Control.Arrow (Arrow (first, second, (***)))
import Control.Monad ((<=<))
import Data.List (sortOn)
import qualified Data.Set as S

type Point = (Int, Int)

data Fold = Vert Int | Horz Int deriving (Show)

readPoint :: [Char] -> Point
readPoint = (read *** read) . second tail . span (/= ',')

pointsFromFile :: FilePath -> IO [Point]
pointsFromFile = return . map readPoint . takeWhile (/= "") . lines <=< readFile

readFold :: String -> Fold
readFold =
  let readFold' ('x' : '=' : n) = Horz $ read n
      readFold' ('y' : '=' : n) = Vert $ read n
      readFold' _ = undefined
   in readFold' . last . splitBy (== ' ')

foldsFromFile :: FilePath -> IO [Fold]
foldsFromFile = return . map readFold . drop 1 . dropWhile (/= "") . lines <=< readFile

transformPoint :: Fold -> Point -> Point
transformPoint (Horz xf) = first (\x -> if x > xf then 2 * xf - x else x)
transformPoint (Vert yf) = second (\y -> if y > yf then 2 * yf - y else y)

part1 :: [Point] -> [Fold] -> Int
part1 pts folds = length . S.map (transformPoint $ head folds) . S.fromList $ pts

part2 :: [Point] -> [Fold] -> String
part2 pts =
  unlines
    . map (take 80 . map ((" #" !!) . fromEnum) . zipWith elem [0 ..] . repeat . map fst)
    . groupOn snd
    . sortOn snd
    . S.toList
    . foldl (\ps f -> S.map (transformPoint f) ps) (S.fromList pts)

main :: IO ()
main = do
  let fp = "Day13/input.txt"
  pts <- pointsFromFile fp
  folds <- foldsFromFile fp
  putStr "Part 1: "
  print $ part1 pts folds
  putStrLn "Part 2: "
  putStrLn $ part2 pts folds