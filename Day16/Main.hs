module Day16.Main where

import Control.Monad (guard, replicateM)
import Data.List (singleton)
import Numeric (readHex)
import Text.ParserCombinators.ReadP (ReadP, get, many, readP_to_S, (<++))

type Bit = Char

type BitArray = [Bit]

data Packet = Literal {getVer :: Int, getVal :: Int} | Operator {getVer :: Int, getOp :: Int, getPackets :: [Packet]} deriving (Show)

bitArrayFromFile :: FilePath -> IO BitArray
bitArrayFromFile path = do
  raw <- readFile path
  let bin = decToBin . fromInteger . fst . head . readHex $ raw
  let leadingZeros = replicate (4 * length raw - length bin) '0'
  return $ leadingZeros ++ bin

binToDec :: BitArray -> Int
binToDec = sum . zipWith (*) (map (2 ^) [0 ..]) . reverse . map (read . singleton)

decToBin :: Integer -> BitArray
decToBin = reverse . map (("01" !!) . fromIntegral . (`mod` 2)) . takeWhile (/= 0) . iterate (`div` 2)

nBitsP :: Int -> ReadP BitArray
nBitsP = flip replicateM get

literalBitsP :: ReadP BitArray
literalBitsP = do
  lastOne <- (== '0') <$> get
  if lastOne then nBitsP 4 else (++) <$> nBitsP 4 <*> literalBitsP

literalP :: ReadP Packet
literalP = do
  ver <- binToDec <$> nBitsP 3
  op <- binToDec <$> nBitsP 3
  guard $ op == 4
  Literal ver . binToDec <$> literalBitsP

operatorP :: ReadP Packet
operatorP = do
  ver <- binToDec <$> nBitsP 3
  op <- binToDec <$> nBitsP 3
  lenType <- get
  if lenType == '1'
    then do
      len <- binToDec <$> nBitsP 11
      ps <- replicateM len packetP
      return $ Operator ver op ps
    else do
      len <- binToDec <$> nBitsP 15
      ps <- fst . last . readP_to_S (many packetP) <$> nBitsP len
      return $ Operator ver op ps

packetP :: ReadP Packet
packetP = literalP <++ operatorP

part1 :: BitArray -> Int
part1 =
  let verCount (Operator ver _ ps) = ver + sum (map verCount ps)
      verCount (Literal ver _) = ver
   in verCount . fst . head . readP_to_S packetP

part2 :: BitArray -> Int
part2 =
  let eval (Literal _ val) = val
      eval (Operator _ 0 ps) = sum . map eval $ ps
      eval (Operator _ 1 ps) = product . map eval $ ps
      eval (Operator _ 2 ps) = minimum . map eval $ ps
      eval (Operator _ 3 ps) = maximum . map eval $ ps
      eval (Operator _ 5 ps) = fromEnum $ eval (head ps) > eval (last ps)
      eval (Operator _ 6 ps) = fromEnum $ eval (head ps) < eval (last ps)
      eval (Operator _ 7 ps) = fromEnum $ eval (head ps) == eval (last ps)
   in eval . fst . head . readP_to_S packetP

main :: IO ()
main = do
  ba <- bitArrayFromFile "Day16/input.txt"
  putStr "Part 1: "
  print $ part1 ba
  putStr "Part 2: "
  print $ part2 ba