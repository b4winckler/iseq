module Fasta (
    Fasta(..)
  , readFasta
  , showFasta
  ) where

import Data.Maybe (isJust, fromJust)
import qualified Data.ByteString.Lazy.Char8 as B


type Header   = B.ByteString
type Sequence = B.ByteString
type Quality  = B.ByteString

data Fasta = Fasta {
    fastaHeader   :: Header
  , fastaSequence :: Sequence
  , fastaQuality  :: Maybe Quality
  } deriving (Show)


-- This function assumes that the sequence and quality parts of the FASTQ file
-- are not split across multiple lines (as should be the case with MiSeq FASTQ
-- files)
parseFastq :: B.ByteString -> [Fasta]
parseFastq = go . B.lines
  where
    go (hdr:rd:_:qual:bs) = Fasta hdr rd (Just qual) : go bs
    go _ = []


parseFasta :: B.ByteString -> [Fasta]
parseFasta = go . B.lines
  where
    go (hdr:rd:bs) = Fasta hdr rd Nothing : go bs
    go _ = []


-- Read file lazily and parse it as FASTA or FASTQ
readFasta :: FilePath -> IO [Fasta]
readFasta path = do
  bs <- B.readFile path
  return $ if not (B.null bs) && B.head bs == '>'
              then parseFasta bs
              else parseFastq bs


showFasta :: Fasta -> B.ByteString
showFasta (Fasta h s q)
  | isJust q  = B.unlines [h, s, B.pack "+", fromJust q]
  | otherwise = B.unlines [h, s]
