module Fasta (
    Fasta
  , fastaHeader
  , fastaQuality
  , fastaSequence
  , readFasta
  , showFasta
  ) where

-- import Data.List (groupBy, maximumBy, group, sort, transpose)
-- import Data.Function (on)
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

-- type Bin = [Entry]
-- type Barcode = B.ByteString


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

-- NB. Sanger encoding
-- encodePhred :: Int -> Char
-- encodePhred q = toEnum $ q + 33

-- NB. Sanger encoding
-- decodePhred :: Char -> Int
-- decodePhred c = fromEnum c - 33

-- histogram :: B.ByteString -> [(Char, Int)]
-- histogram = map f . group . sort . B.unpack
--   where
--     f bs = (head bs, fromIntegral $ length bs)

-- A bin is defined by the first 16 basepairs of a FASTQ entry
-- bins :: [Entry] -> [Bin]
-- bins = groupBy binid
--   where
--     binid (Fastq _ r1 _) (Fastq _ r2 _) = B.take 16 r1 == B.take 16 r2
--     binid (Fasta _ r1)   (Fasta _ r2)   = B.take 16 r1 == B.take 16 r2
--     binid _ _ = False

-- binSize :: Bin -> (Int, Barcode)
-- binSize ((Fastq _ r _):fqs) = (1 + length fqs, B.take 16 r)
-- binSize ((Fasta _ r):fqs)   = (1 + length fqs, B.take 16 r)
-- binSize _                   = (0, B.empty)

-- consensus :: Bin -> (String, [Int])
-- consensus = unzip
--           . map (maximumBy (compare `on` snd) . histogram)
--           . B.transpose
--           . map fastqSequence

-- qconsensus :: Bin -> (String, [(Int,Int)])
-- qconsensus fqs = undefined
  -- where
    -- xs = map (\fq -> B.zip $ fastqSequence fq $ fastqQuality fq) fqs
    -- ys = transpose xs

-- chooseBase :: [(Char, Char)] -> (Char, Int)
-- chooseBase = undefined

-- Outer list corresponds to basepair 1, 2, ..., etc
-- Inner list corresponds to (base, quality) for FASTQ 1, 2, ..., etc
-- transposeBin :: Bin -> [[(Char, Char)]]
-- transposeBin = transpose
--              . map (\fq -> B.zip (fastqSequence fq) (fastqQuality fq))

-- Pick out header to represent all reads in the bin
-- representativeHeader :: Bin -> Header
-- representativeHeader ((Fastq hdr _ _):_) = hdr
-- representativeHeader ((Fasta hdr _):_) = hdr
-- representativeHeader _ = B.pack ">no_header"

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
