module Common where

import Control.Arrow
import Data.List
import Data.Maybe

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