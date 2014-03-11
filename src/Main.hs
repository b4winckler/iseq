module Main where

import Control.Applicative ((<$>), (<*>), pure)
import Options.Applicative ((<>), optional, Parser, command, info,
                            execParser, strOption, help, switch, help, helper,
                            fullDesc, progDesc, long, header, str, option,
                            subparser, flag, value, argument, metavar)

import Options
import Scan (scan)



main :: IO ()
main = do
  o <- execParser (info (helper <*> parser)
      (fullDesc <> header "Tools for processing Illumina ?iSeq data"))
  runAction (optCommand o) o


parser :: Parser IseqOptions
parser = IseqOptions
  <$> switch (long "verbose" <> help "Be verbose")
  <*> optional (strOption (long "output" <> help "Output path"))
  <*> subparser (
        command "align" (info alignOptParser $
            progDesc "Align against database")
    <>  command "merge" (info mergeOptParser $ progDesc "Merge paired reads")
    <>  command "scan" (info scanOptParser $ progDesc "Scan for primer")
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
  <*> option (long "position" <> value 0 <> metavar "N"
      <> help "Primer start is located within N bases")
  <*> option (long "errors" <> value 0 <> metavar "N"
      <> help "Allow at most N primer errors")
  <*> option (long "skip" <> value 0 <> metavar "N"
      <> help "Skip first N bases before starting scan")
  <*> argument str (metavar "PRIMER" <> help "Primer sequence to scan for")
  <*> pure scan
