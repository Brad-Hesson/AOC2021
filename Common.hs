module Common where

import Control.Arrow (Arrow ((***)))
import Data.Array (Array, bounds, elems)
import Data.List (groupBy)
import Data.Maybe (fromJust, isNothing)

mapF :: [a -> b] -> a -> [b]
mapF lfs = flip map lfs . flip ($)

iterMaybe :: (a -> Maybe a) -> a -> a
iterMaybe f a
  | isNothing . f $ a = a
  | otherwise = iterMaybe f . fromJust . f $ a

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = filter (not . f . head) . groupOn f

groupOn :: Eq e => (a -> e) -> [a] -> [[a]]
groupOn f = groupBy (curry $ uncurry (==) . (f *** f))

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n l = take n l : chop n (drop n l)

print2DArray :: Show a => Array (Int, Int) a -> IO ()
print2DArray mz =
  let width = snd . snd . bounds $ mz
   in putStrLn . unlines . map concat . chop width . map show . elems $ mz