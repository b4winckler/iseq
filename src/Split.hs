{-# LANGUAGE OverloadedStrings #-}
module Split (split) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import System.IO (stderr, hPutStrLn)
import Data.List (sort, group)
import qualified Data.ByteString.Lazy.Char8 as B

import Options
import Fasta

type Barcode = B.ByteString


split :: IseqOptions -> IO ()
split opt = do
  let lopt      = optCommand opt
      opath     = optInput lopt
      obarcodes = optBarcodes lopt

  bc2name <- map (\x -> (fastaSequence x, fastaHeader x)) <$>
             readFasta obarcodes

  entries <- readFasta opath
  let ext = fileExtension entries

  fnames <- forM entries $ \entry -> do
    -- TODO: B.tail will fail if header is empty string
    let sample = maybe "unkown" B.tail $ lookup (fastaBarcode entry) bc2name
        fname  = B.unpack $ B.append sample ext
    B.appendFile fname $ showFasta entry
    return fname

  logStrLn "Number of reads and filenames that were written:"
  mapM_ (logStrLn . showStats) $ collate fnames

  where
    showStats (fn,c) = let sc = show c
                       in "    " ++ sc ++ replicate (12 - length sc) ' ' ++ fn



-- TODO: must ensure that fasta header ends with ':barcode'
fastaBarcode :: Fasta -> Barcode
fastaBarcode = last . B.split ':' . fastaHeader


fileExtension :: [Fasta] -> B.ByteString
fileExtension (fa:_) = case (fastaQuality fa) of
                         (Just _) -> ".fastq"
                         Nothing  -> ".fasta"
fileExtension _      = ".fasta"


logStrLn :: String -> IO ()
logStrLn = hPutStrLn stderr


collate :: [String] -> [(String, Int)]
collate = map (\xs -> (head xs, length xs)) . group . sort
