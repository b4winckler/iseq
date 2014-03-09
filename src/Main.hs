module Main where

import Control.Applicative ((<$>), (<*>), pure)
import Options.Applicative ((<>), optional)
import Options.Applicative (Parser, command, info,
                            execParser, strOption, help, switch, help, helper,
                            fullDesc, progDesc, long, header, str,
                            subparser, flag, value, argument, metavar)

import Options
import Scan (scan)



main :: IO ()
main = do
  o <- execParser (info (helper <*> parser)
      (fullDesc <> header "Tools for processing Illumina ?iSeq data"))
  print o

  case optCommand o of
    CmdScan input primer -> scan o input primer
    _                    -> return ()


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

mergeOptParser :: Parser Command
mergeOptParser = pure CmdMerge

scanOptParser :: Parser Command
scanOptParser = CmdScan
  <$> strOption (long "input" <> value "/dev/stdin"
      <> help "Fasta file to scan")
  <*> argument str (metavar "PRIMER" <> help "Primer sequence to scan for")
