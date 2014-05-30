{-# LANGUAGE BangPatterns #-}

module Scan (scan) where

import Data.List (tails, minimumBy)
import Data.Ord (comparing)
import Control.Monad (forM_, when)
import qualified Data.ByteString.Lazy.Char8 as B

import Options
import Fasta


-- Find best match starting within 'shift' elements from the beginning of 'y'
-- that minimizes the edit distance to 'x'.  If edit distance exceeds 'maxErr'
-- then abort the search.
bestMatch :: Eq a => Int -> Int -> [a] -> [a] -> (Int, Int, Int)
bestMatch maxErr shift primer y = (nerr, nshift, nmatch)
  where
    (nshift,(nerr,nmatch)) = minimumBy (comparing (fst . snd)) $ zip [0..] shifts
    shifts = map (editDist e primer) (take k $ tails y)
    e = max 0 maxErr
    k = max 1 (shift + 1)


-- Calculate edit distance to 'primer'.  If edit distance exceeds 'maxErr' then
-- abort the search.  The time complexity in 'maxErr' is exponential in the
-- current implementation, so choose it small (e.g. 2).
-- Return '(nerr,nmatch)', where 'nerr' is the number of errors and 'nmatch' is
-- the number of elements that "matched" the primer (up to
-- substitution/insertion/deletion).
editDist :: Eq a => Int -> [a] -> [a] -> (Int, Int)
editDist maxErr primer = go 0 0 primer
  where
    go !nerr !nmatch [] _ = (nerr, nmatch)
    go !nerr !nmatch xs [] = (nerr + length xs, nmatch)
    go !nerr !nmatch xa@(x:xs) ya@(y:ys)
        | nerr > maxErr = (nerr, nmatch)            -- give up
        | x == y = go nerr (nmatch+1) xs ys         -- exact match
        | otherwise = minimumBy (comparing fst) [
              go (nerr+1) (nmatch+1) xs ys          -- substitution
            , go (nerr+1) nmatch     xs ya          -- insertion
            , go (nerr+1) (nmatch+1) xa ys ]        -- deletion


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
    let sequence = B.unpack $ fastaSequence entry
        (nerr, nshift, nmatch) = bestMatch oerr oshift oprimer
            $ drop oskip sequence
    when (nerr <= oerr) $ do
      let hdr = B.unwords [
                fastaHeader entry
              , keyval "skip" $ take oskip sequence
              , keyval "shift" $ take nshift $ drop oskip sequence
              , keyval "primer" $ take nmatch $ drop (oskip+nshift) sequence
              , keyval "primer_err" $ show nerr]
          sqn = B.pack $ drop (oskip+nshift+nmatch) sequence
          qual = fmap (B.drop (fromIntegral $ oskip+nshift+nmatch)) (fastaQuality entry)

      B.putStr $ showFasta $ Fasta hdr sqn qual


keyval :: String -> String -> B.ByteString
keyval k v = B.pack $ k ++ "=" ++ v
