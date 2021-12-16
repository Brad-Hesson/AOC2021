module Day8.Main where

import Control.Arrow (Arrow ((&&&), (***)))
import Control.Monad ((<=<))
import Data.List (groupBy, nub, partition, sortOn)
import qualified Data.Map as M
import qualified Data.Set as S

type Instance = ([Digit], [Digit])

type Digit = S.Set Char

type SegMap = M.Map Char (S.Set Char)

parseInstance :: String -> Instance
parseInstance = (map S.fromList . words *** map S.fromList . words . tail) . break (== '|')

instancesFromFile :: FilePath -> IO [Instance]
instancesFromFile = return . map parseInstance . lines <=< readFile

buildSegmap :: [Digit] -> M.Map Char Digit
buildSegmap ds = M.unionsWith S.intersection . map (M.fromList . uncurry zip . (S.toList &&& repeat . S.fromList . options)) $ ds
  where
    d1 = (!! 0) . nub . sortOn length $ ds
    d7 = (!! 1) . nub . sortOn length $ ds
    d4 = (!! 2) . nub . sortOn length $ ds
    options digit
      | length digit == 2 = "cf"
      | length digit == 3 = "acf"
      | length digit == 4 = "bcdf"
      | length digit == 5 && d1 `S.isSubsetOf` digit = "acdfg"
      | length digit == 5 = "abcdefg"
      | length digit == 6 && d4 `S.isSubsetOf` digit = "abcdfg"
      | length digit == 6 && d7 `S.isSubsetOf` digit = "abcefg"
      | length digit == 6 = "abdefg"
      | length digit == 7 = "abcdefg"
      | otherwise = undefined

digitToInt :: Digit -> Int
digitToInt d
  | d == S.fromList "abcefg" = 0
  | d == S.fromList "cf" = 1
  | d == S.fromList "acdeg" = 2
  | d == S.fromList "acdfg" = 3
  | d == S.fromList "bcdf" = 4
  | d == S.fromList "abdfg" = 5
  | d == S.fromList "abdefg" = 6
  | d == S.fromList "acf" = 7
  | d == S.fromList "abcdefg" = 8
  | d == S.fromList "abcdfg" = 9
  | otherwise = undefined

reduceSegmap :: SegMap -> [(Char, Char)]
reduceSegmap m
  | M.null m = []
  | otherwise = donelist ++ reduceSegmap (M.map f rest)
  where
    donelist = M.toList . M.map ((!! 0) . S.toList) $ dones
    f = flip S.difference . S.unions . M.elems $ dones
    (dones, rest) = M.partition ((== 1) . length) m

solveInstance :: Instance -> Int
solveInstance inst = sum . zipWith (*) (map (10 ^) [0 ..]) . reverse . map solveSegmap . snd $ inst
  where
    solveSegmap = digitToInt . S.map ((M.!) . M.fromList . reduceSegmap . buildSegmap . fst $ inst)

part1 :: [Instance] -> Int
part1 = sum . map (length . fst . partition (`elem` [2, 3, 4, 7]) . map length . snd)

part2 :: [Instance] -> Int
part2 = sum . map solveInstance

main :: IO ()
main = do
  insts <- instancesFromFile "Day8/input.txt"
  putStr "Part 1: "
  print $ part1 insts
  putStr "Part 2: "
  print $ part2 insts