{-# LANGUAGE CPP #-}
module Main where

import Data.Version (showVersion)
import Options.Applicative

import Options
import Paths_iseq (version)
import Strip (strip)
import Split (split)

#if __GLASGOW_HASKELL__ <= 702
import Data.Monoid
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif


main :: IO ()
main = do
  o <- execParser (info (helper <*> versionParser <*> parser) fullDesc)
  runAction (optCommand o) o


versionParser :: Parser (a -> a)
versionParser = infoOption ("iseq v" ++ showVersion version)
    (long "version" <> help "Show version")


parser :: Parser IseqOptions
parser = IseqOptions
  <$> switch (long "verbose" <> help "Be verbose")
  <*> subparser (
        command "align" (info (helper <*> alignOptParser) $
            progDesc "Align against database")
    <>  command "merge" (info (helper <*> mergeOptParser) $
            progDesc "Merge paired reads")
    <>  command "strip" (info (helper <*> stripOptParser) $
            progDesc "Strip primer from input")
    <> command "split" (info (helper <*> splitOptParser) $
            progDesc "Split by sample barcodes")
    )


alignOptParser :: Parser Command
alignOptParser = CmdAlign
  <$> flag GlobalAlignment LocalAlignment (
        long "local" <> help "local alignment")
  <*> pure undefined


mergeOptParser :: Parser Command
mergeOptParser = CmdMerge <$> pure undefined


stripOptParser :: Parser Command
stripOptParser = CmdStrip
  <$> strOption (long "input" <> value "/dev/stdin" <> metavar "PATH"
      <> help "Fasta file to strip primers from")
  <*> option (long "shift" <> value 0 <> metavar "N"
      <> help "Allow primer position to be shifted up to N bases")
  <*> option (long "errors" <> value 0 <> metavar "N"
      <> help "Allow at most N primer errors")
  <*> option (long "skip" <> value 0 <> metavar "N"
      <> help "Skip first N bases before starting primer scan")
  <*> argument str (metavar "PRIMER" <> help "Primer sequence to scan for")
  <*> pure strip

splitOptParser :: Parser Command
splitOptParser = CmdSplit
  <$> strOption (long "input" <> value "/dev/stdin" <> metavar "PATH"
      <> help "Fasta file to split [stdin]")
  <*> optional (strOption (long "output" <> metavar "PATH"
      <> help "Output path"))
  <*> strOption (long "barcodes" <> metavar "PATH"
      <> help "Fasta file with sample barcodes")
  <*> pure split
