module Day21.Main where

import Common
import Control.Arrow
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

class Die d where
  rollDie :: d -> (d, Int)

data Player = Player {getPos :: Int, getScore :: Int} deriving (Eq, Ord)

instance Show Player where
  show p = "Player {getPos = " ++ show (1 + getPos p) ++ ", getScore = " ++ show (getScore p) ++ "}"

data Game d = Game {getP0 :: Player, getP1 :: Player, getActivePlayer :: Int, getDie :: d} deriving (Show)

instance Eq (Game d) where
  (Game p01 p11 pa1 _) == (Game p02 p12 pa2 _) = (p01 == p02) && (p11 == p12) && (pa1 == pa2)

instance Ord (Game d) where
  compare (Game p01 p11 pa1 _) (Game p02 p12 pa2 _) = case compare p01 p02 of
    LT -> LT
    GT -> GT
    EQ -> case compare p11 p12 of
      LT -> LT
      GT -> GT
      EQ -> compare pa1 pa2

data DtrmDie = DtrmDie Int Int deriving (Show)

instance Die DtrmDie where
  rollDie (DtrmDie val count) =
    let val' = (val + 3) `mod` 100
        count' = count + 3
        roll = (+ 3) . sum . take 3 . map (`mod` 100) $ [val ..]
     in (DtrmDie val' count', roll)

data ForcedDie = ForcedDie Int | SpentDie Int deriving (Show)

instance Die ForcedDie where
  rollDie (ForcedDie roll) = (SpentDie roll, roll)
  rollDie (SpentDie _) = undefined

newtype Tally = Tally {getTally :: [Int]} deriving (Show)

startingFromFile :: FilePath -> IO [Int]
startingFromFile = return . map (read . last . splitBy (== ':')) . lines <=< readFile

gameFromStartingAndDie :: Die d => [Int] -> d -> Game d
gameFromStartingAndDie [p0, p1] d = Game (Player (p0 - 1) 0) (Player (p1 - 1) 0) 0 d
gameFromStartingAndDie _ _ = undefined

updatePlayer :: Int -> Player -> Player
updatePlayer roll player =
  let pos' = (getPos player + roll) `mod` 10
      sc' = (getScore player + pos' + 1)
   in Player pos' sc'

stepGame :: (Show d, Die d) => Game d -> Game d
stepGame (Game p0 p1 pa die) =
  let (die', roll) = rollDie die
      p0' = if pa == 0 then updatePlayer roll p0 else p0
      p1' = if pa == 1 then updatePlayer roll p1 else p1
      pa' = 1 - pa
   in Game p0' p1' pa' die'

scoresFromGame :: Game d -> [Int]
scoresFromGame (Game p0 p1 _ _) = [getScore p0, getScore p1]

rollCountFromGame :: Game DtrmDie -> Int
rollCountFromGame (Game _ _ _ (DtrmDie _ count)) = count

winnerTallyFromGame :: Int -> Game d -> Maybe Tally
winnerTallyFromGame n g =
  if (>= n) . maximum . scoresFromGame $ g
    then Just $ if getActivePlayer g == 1 then Tally [1, 0] else Tally [0, 1]
    else Nothing

playToScore :: (Show d, Die d) => Int -> Game d -> Game d
playToScore n = head . dropWhile (isNothing . winnerTallyFromGame n) . iterate stepGame

combineTallys :: [Tally] -> Tally
combineTallys = Tally . map sum . transpose . ([0, 0] :) . map getTally

tallyFromGame :: Game ForcedDie -> Tally
tallyFromGame =
  let tfg m g = case M.lookup g m of
        Just tally -> (m, tally)
        Nothing -> case winnerTallyFromGame 21 g of
          Just tally -> (m, tally)
          Nothing ->
            let games = [stepGame g {getDie = ForcedDie (s0 + s1 + s2)} | s0 <- [1, 2, 3], s1 <- [1, 2, 3], s2 <- [1, 2, 3]]
                f (m, ts) g = (M.insert g t m', t : ts) where (m', t) = tfg m g
                (m', tallys) = foldl f (m, []) games
             in (m', combineTallys tallys)
   in snd . tfg M.empty

part1 :: [Int] -> Int
part1 starting =
  let final = playToScore 1000 $ gameFromStartingAndDie starting (DtrmDie 0 0)
   in rollCountFromGame final * minimum (scoresFromGame final)

part2 :: [Int] -> Int
part2 starting = maximum . getTally . tallyFromGame $ gameFromStartingAndDie starting (ForcedDie 0)

main :: IO ()
main = do
  starting <- startingFromFile "Day21/input.txt"
  putStr "Part 1: "
  print $ part1 starting
  putStr "Part 2: "
  print $ part2 starting