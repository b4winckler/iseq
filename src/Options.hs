module Options (
    IseqOptions(..)
  , Command(..)
  , Alignment(..)
  ) where


data IseqOptions = IseqOptions {
      optVerbose :: Bool
    , optOutput  :: Maybe FilePath
    , optCommand :: Command
    } deriving (Show)

data Command =
    CmdAlign { optAlignment :: Alignment }
  | CmdMerge
  | CmdScan {
        optInput  :: FilePath
      , optPrimer :: String
      }
  deriving (Show)

data Alignment =
    GlobalAlignment
  | LocalAlignment
  deriving (Show, Eq)

