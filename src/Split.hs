{-# LANGUAGE OverloadedStrings #-}
module Split (split) where

import Control.Exception.Base (bracket)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing)
import System.IO (openFile, IOMode (..), hClose)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified System.FilePath as Path

import Options
import Fasta

type Barcode = B.ByteString


split :: IseqOptions -> IO ()
split opt = do
  let lopt = optCommand opt

  entries <- readFasta $ optInput lopt
  bcfasta <- readFasta $ optBarcodes lopt

  let barcodes = map fastaSequence bcfasta
      sampleNames = map (B.tail . fastaHeader) bcfasta
      ext = fileExtension entries
      fileNames = map (B.unpack . (`B.append` ext)) sampleNames
      unknownName = B.unpack $ B.append "unknown" ext

  -- Open all file handles before writing to avoid having to reopen them all
  -- the time.  The downside to this is that a file is created for each sample,
  -- even if they are not present in the input.
  let opathout  = optOutput lopt
  withMaybe opathout $ createDirectoryIfMissing True

  let fullNames = map (prependPath opathout) (unknownName:fileNames)
  withFiles fullNames $ \handles -> do
    let bc2fh = zip barcodes $ tail handles
    forM_ entries $ \entry -> do
      let bc = fastaBarcode entry
          fh = fromMaybe (head handles) $ lookup bc bc2fh
      B.hPut fh $ showFasta entry

  where
    withFiles fnames =
      bracket (mapM (flip openFile WriteMode) fnames) (mapM_ hClose)


withMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
withMaybe m f = case m of
  Just a  -> f a
  Nothing -> return ()

fastaBarcode :: Fasta -> Barcode
fastaBarcode = last . B.split ':' . fastaHeader

fileExtension :: [Fasta] -> B.ByteString
fileExtension (fa:_) = case (fastaQuality fa) of
                         (Just _) -> ".fastq"
                         Nothing  -> ".fasta"
fileExtension _      = ".fasta"

prependPath :: Maybe FilePath -> FilePath -> FilePath
prependPath base file = maybe file (flip Path.combine file) base
