module Day4.Main where

import Data.List
import Control.Arrow
import Control.Monad

type Board = [[Int]]

dataFromFile :: FilePath -> IO ([Board], [Int])
dataFromFile = return . (parseBoards . tail &&& parseNumbers . head) . lines <=< readFile

parseNumbers :: String -> [Int]
parseNumbers = map read . filter (/=",") . groupBy (\a b-> (a==',') == (b==','))

parseBoards :: [String] -> [Board]
parseBoards = map (map (map read . words)) . filter (/=[""]) . groupBy (\a b-> (a=="") == (b==""))

updateBoards :: Int -> [Board] -> [Board]
updateBoards n = map (map (map (\x->if x==n then 0 else x)))

isBoardWin :: Board -> Bool
isBoardWin = (0 `elem`) . map sum . uncurry (++) . (id &&& transpose)

generateGame :: ([Board], [Int]) -> [([Board],Int)]
generateGame = uncurry (scanl f) . (flip (,) 0 *** id) where
  f (bs,_) n = (updateBoards n bs,n)

part1 :: ([Board], [Int]) -> Int
part1 = uncurry (*)
  . first (sum . map (\b-> if isBoardWin b then sum . concat $ b else 0))
  . head
  . dropWhile (and . map (not . isBoardWin) . fst)
  . generateGame

part2 :: ([Board], [Int]) -> Int
part2 = uncurry (*)
  . first (sum . concat)
  . (\(i,(bs,n))-> (bs!!i,n))
  . (sum . zipWith (*) [0..] . map (fromEnum . not . isBoardWin) . fst . last *** head)
  . span (or . map (not . isBoardWin) . fst)
  . generateGame

main :: IO ()
main = do
  gameData <- dataFromFile "Day4/input.txt"
  putStr "Part 1: "
  print . part1 $ gameData
  putStr "Part 2: "
  print . part2 $ gameData