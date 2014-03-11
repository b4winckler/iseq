{-# LANGUAGE BangPatterns #-}

module Scan (scan) where

import Data.List (tails, minimumBy)
import Data.Ord (comparing)
import Control.Monad (forM_, when)
import qualified Data.ByteString.Lazy.Char8 as B

import Options
import Fasta


-- Find best match within 'shift' elements from the start of 'y' that minimizes
-- the edit distance to 'x'.  If edit distance exceeds 'maxErr' then abort the
-- search.
bestMatch :: Eq a => Int -> Int -> [a] -> [a] -> (Int, Int)
bestMatch maxErr shift x y = minimumBy (comparing snd) $ zip [1..] distances
  where
    distances = map (editDist e x) (take k $ tails y)
    e         = max 0 maxErr
    k         = max 1 (shift + 1)


-- Calculate edit distance between 'x' and 'y'.  If edit distance exceeds
-- 'maxErr' then abort the search.  The time complexity in 'maxErr' is
-- exponential in the current implementation, so choose it small (e.g. 2).
editDist :: Eq a => Int -> [a] -> [a] -> Int
editDist maxErr = go 0
  where
    go !n []        _         = n
    go !n xs        []        = n + length xs
    go !n xa@(x:xs) ya@(y:ys)
        | n > maxErr  = n
        | x == y      = go n xs ys
        | otherwise   = minimum [ go (n+1) xa ys   -- deletion
                                , go (n+1) xs ya   -- insertion
                                , go (n+1) xs ys ] -- substitution


scan :: IseqOptions -> IO ()
scan opt = do
  let lopt    = optCommand opt
      opath   = optInput lopt
      oprimer = optPrimer lopt
      oshift  = optShift lopt
      oerr    = optErrors lopt
      oskip   = max 0 $ optSkip lopt

  entries <- readFasta opath
  forM_ entries $ \entry -> do
    let sequence   = drop oskip $ B.unpack $ fastaSequence entry
        (p, score) = bestMatch oerr oshift oprimer sequence
        pos        = p + oskip
    when (score <= oerr) $ do
      let out = entry { fastaHeader = B.unwords [
            fastaHeader entry
          , keyval "primer-pos" (show pos)
          , keyval "primer-err" (show score)
          ] }
      B.putStr (showFasta out)


keyval :: String -> String -> B.ByteString
keyval k v = B.pack $ k ++ "=" ++ v
