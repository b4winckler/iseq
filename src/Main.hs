{-# LANGUAGE CPP #-}
module Main where

import Data.Version (showVersion)
import Options.Applicative

import Options
import Paths_iseq (version)
import Scan (scan)

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
  <*> optional (strOption (long "output" <> help "Output path"))
  <*> subparser (
        command "align" (info (helper <*> alignOptParser) $
            progDesc "Align against database")
    <>  command "merge" (info (helper <*> mergeOptParser) $
            progDesc "Merge paired reads")
    <>  command "scan" (info (helper <*> scanOptParser) $
            progDesc "Scan for primer")
    )


alignOptParser :: Parser Command
alignOptParser = CmdAlign
  <$> flag GlobalAlignment LocalAlignment (
        long "local" <> help "local alignment")
  <*> pure undefined


mergeOptParser :: Parser Command
mergeOptParser = CmdMerge <$> pure undefined


scanOptParser :: Parser Command
scanOptParser = CmdScan
  <$> strOption (long "input" <> value "/dev/stdin" <> metavar "PATH"
      <> help "Fasta file to scan")
  <*> option (long "shift" <> value 0 <> metavar "N"
      <> help "Allow primer position to be shifted up to N bases")
  <*> option (long "errors" <> value 0 <> metavar "N"
      <> help "Allow at most N primer errors")
  <*> option (long "skip" <> value 0 <> metavar "N"
      <> help "Skip first N bases before starting scan")
  <*> argument str (metavar "PRIMER" <> help "Primer sequence to scan for")
  <*> pure scan
