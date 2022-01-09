{-# LANGUAGE TupleSections #-}

module Day16.Main where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Arrow (Arrow (first))
import Control.Monad (guard, replicateM)
import Data.Either (isRight)
import Numeric (readHex)

type Bit = Bool

type BitArray = [Bit]

data Packet
  = Literal {getVer :: Int, getVal :: Int}
  | Operator {getVer :: Int, getOp :: Int, getPackets :: [Packet]}
  deriving (Show)

bitArrayFromFile :: FilePath -> IO BitArray
bitArrayFromFile path = do
  raw <- readFile path
  let bin = decToBin . fromInteger . fst . head . readHex $ raw
  let leadingZeros = replicate (4 * length raw - length bin) False
  return $ leadingZeros ++ bin

binToDec :: BitArray -> Int
binToDec = sum . zipWith (*) (map (2 ^) [0 ..]) . reverse . map fromEnum

decToBin :: Integer -> BitArray
decToBin = reverse . map ((== 1) . (`mod` 2)) . takeWhile (/= 0) . iterate (`div` 2)

newtype Parser a = Parser {parse :: BitArray -> Either String (a, BitArray)}

instance Functor Parser where fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
  pure a = Parser $ Right . (a,)
  (Parser p1) <*> (Parser p2) = Parser $ \ba -> do
    (f, ba') <- p1 ba
    (a, ba'') <- p2 ba'
    return (f a, ba'')

instance Monad Parser where
  (Parser p1) >>= f = Parser $ \ba -> do
    (a, ba') <- p1 ba
    parse (f a) ba'

instance Alternative Parser where
  empty = Parser $ const (Left "Error: Alternative Failed")
  p1 <|> p2 = Parser $ \ba -> case parse p1 ba of
    Left _ -> parse p2 ba
    Right res -> Right res

oneBitP :: Parser Bit
oneBitP =
  let p [] = Left "Error: EOF"
      p (b : bs) = Right (b, bs)
   in Parser p

nBitsP :: Int -> Parser BitArray
nBitsP = flip replicateM oneBitP

literalBitsP :: Parser BitArray
literalBitsP = do
  isThereMore <- oneBitP
  if not isThereMore then nBitsP 4 else (++) <$> nBitsP 4 <*> literalBitsP

literalP :: Parser Packet
literalP = do
  ver <- binToDec <$> nBitsP 3
  op <- binToDec <$> nBitsP 3
  guard $ op == 4
  Literal ver . binToDec <$> literalBitsP

operatorP :: Parser Packet
operatorP = do
  ver <- binToDec <$> nBitsP 3
  op <- binToDec <$> nBitsP 3
  lenType <- oneBitP
  if lenType
    then do
      len <- binToDec <$> nBitsP 11
      ps <- replicateM len packetP
      return $ Operator ver op ps
    else do
      len <- binToDec <$> nBitsP 15
      bitarray <- nBitsP len
      let result = parse (many packetP) bitarray
      guard $ isRight result
      let Right (ps, _) = result
      return $ Operator ver op ps

packetP :: Parser Packet
packetP = literalP <|> operatorP

part1 :: BitArray -> Int
part1 ba =
  let Right (ast, _) = parse packetP ba
      verCount (Operator ver _ ps) = ver + sum (map verCount ps)
      verCount (Literal ver _) = ver
   in verCount ast

part2 :: BitArray -> Int
part2 ba =
  let Right (ast, _) = parse packetP ba
      eval (Literal _ val) = val
      eval (Operator _ op ps) = case op of
        0 -> sum . map eval $ ps
        1 -> product . map eval $ ps
        2 -> minimum . map eval $ ps
        3 -> maximum . map eval $ ps
        5 -> fromEnum $ eval (head ps) > eval (last ps)
        6 -> fromEnum $ eval (head ps) < eval (last ps)
        7 -> fromEnum $ eval (head ps) == eval (last ps)
   in eval ast

main :: IO ()
main = do
  ba <- bitArrayFromFile "Day16/input.txt"
  let Right (ast, _) = parse packetP ba
  putStr "Part 1: "
  print $ part1 ba
  putStr "Part 2: "
  print $ part2 ba