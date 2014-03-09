module Main where

import Control.Applicative ((<$>), (<*>), pure)
import Options.Applicative ((<>), optional)
import Options.Applicative (Parser, command, info,
                            execParser, strOption, help, switch, help, helper,
                            fullDesc, progDesc, long, header,
                            subparser, flag)

data IseqOptions = IseqOptions {
      optVerbose :: Bool
    , optOutput  :: Maybe String
    , optCommand :: Command
    } deriving (Show)

data Command =
    CmdAlign { optAlignment :: Alignment }
  | CmdMerge
  deriving (Show)

data Alignment =
    GlobalAlignment
  | LocalAlignment
  deriving (Show, Eq)


main :: IO ()
main = do
  o <- execParser (info (helper <*> parser)
      (fullDesc <> header "Tools for processing Illumina ?iSeq data"))
  print o

parser :: Parser IseqOptions
parser = IseqOptions
  <$> switch (long "verbose" <> help "Be verbose")
  <*> optional (strOption (long "output" <> help "Output path"))
  <*> subparser (
        command "align" (info alignOptParser $
            progDesc "Align against database")
    <>  command "merge" (info mergeOptParser $ progDesc "Merge paired reads")
    )

alignOptParser :: Parser Command
alignOptParser = CmdAlign
  <$> flag GlobalAlignment LocalAlignment (
        long "local" <> help "local alignment")

mergeOptParser :: Parser Command
mergeOptParser = pure CmdMerge
