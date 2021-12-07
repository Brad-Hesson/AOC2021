{-# LANGUAGE TupleSections #-}

module Day4.Main where

import Control.Arrow
import Control.Monad
import Data.List

type Board = [[Int]]

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = filter (not . f . head) . groupBy (curry $ uncurry (==) . (f *** f))

dataFromFile :: FilePath -> IO ([Board], [Int])
dataFromFile = return . (parseBoards . tail &&& parseNumbers . head) . lines <=< readFile

parseNumbers :: String -> [Int]
parseNumbers = map read . splitBy (== ',')

parseBoards :: [String] -> [Board]
parseBoards = map (map (map read . words)) . splitBy (== "")

updateBoards :: Int -> [Board] -> [Board]
updateBoards n = map (map (map (\x -> if x == n then 0 else x)))

isBoardWin :: Board -> Bool
isBoardWin = (0 `elem`) . map sum . uncurry (++) . (id &&& transpose)

generateGame :: ([Board], [Int]) -> [([Board], Int)]
generateGame = uncurry (scanl f) . first (,0)
  where
    f (bs, _) n = (updateBoards n bs, n)

part1 :: ([Board], [Int]) -> Int
part1 =
  uncurry (*)
    . first (sum . map (\b -> if isBoardWin b then sum . concat $ b else 0))
    . head
    . dropWhile (not . any isBoardWin . fst)
    . generateGame

argWhere :: (a -> Bool) -> [a] -> [Int]
argWhere pred = map fst . filter (pred . snd) . zip [0 ..]

part2 :: ([Board], [Int]) -> Int
part2 =
  uncurry (*)
    . first (sum . concat)
    . (\(i, (bs, n)) -> (bs !! i, n))
    . (head . argWhere (not . isBoardWin) . fst . last *** head)
    . break (all isBoardWin . fst)
    . generateGame

main :: IO ()
main = do
  gameData <- dataFromFile "Day4/input.txt"
  putStr "Part 1: "
  print . part1 $ gameData
  putStr "Part 2: "
  print . part2 $ gameData