module Day10.Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad ((<=<))
import Data.List (sort)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)

linesFromFile :: FilePath -> IO [String]
linesFromFile = return . lines <=< readFile

reduceLine :: String -> String
reduceLine = reverse . foldl f []
  where
    f ('(' : rest) ')' = rest
    f ('[' : rest) ']' = rest
    f ('{' : rest) '}' = rest
    f ('<' : rest) '>' = rest
    f l c = c : l

charToPoints :: Char -> Int
charToPoints ')' = 3
charToPoints ']' = 57
charToPoints '}' = 1197
charToPoints '>' = 25137
charToPoints '(' = 1
charToPoints '[' = 2
charToPoints '{' = 3
charToPoints '<' = 4

reducedToCorrupt :: String -> Maybe Char
reducedToCorrupt = listToMaybe . dropWhile (`elem` "([{<")

part1 :: [String] -> Int
part1 = sum . map charToPoints . mapMaybe (reducedToCorrupt . reduceLine)

part2 :: [String] -> Int
part2 =
  uncurry (!!)
    . (id &&& (`div` 2) . length)
    . sort
    . map (foldr (\v acc -> acc * 5 + charToPoints v) 0)
    . filter (isNothing . reducedToCorrupt)
    . map reduceLine

main :: IO ()
main = do
  lines <- linesFromFile "Day10/input.txt"
  putStr "Part 1: "
  print $ part1 lines
  putStr "Part 2: "
  print $ part2 lines