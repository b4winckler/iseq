{-# LANGUAGE BangPatterns #-}

module Scan (bestMatchWithin, editDist, scan) where

import Data.List (tails, minimumBy)
import Data.Ord (comparing)

import Options


-- Find position within n characters from start of y that minimizes edit
-- distance to x.
bestMatchWithin :: Eq a => Int -> [a] -> [a] -> (Int, Int)
bestMatchWithin n x y = minimumBy (comparing snd) $ zip [1..] distances
  where
    distances = map (editDist x) (take (max 1 $ n+1) $ tails y)


-- Edit distance between two lists
editDist :: Eq a => [a] -> [a] -> Int
editDist = go 0
  where
    go !n []        _         = n
    go !n xs        []        = n + length xs
    go !n xa@(x:xs) ya@(y:ys)
        | x == y    = go n xs ys
        | otherwise = minimum [ go (n+1) xa ys   -- deletion
                              , go (n+1) xs ya   -- insertion
                              , go (n+1) xs ys ] -- substitution


scan = undefined
