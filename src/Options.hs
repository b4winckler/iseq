module Options (
    IseqOptions(..)
  , Command(..)
  ) where


data IseqOptions = IseqOptions {
      optCommand :: Command
    }

type CmdAction = IseqOptions -> IO ()

data Command =
    CmdStrip {
        optInput   :: FilePath
      , optShift   :: Int
      , optErrors  :: Int
      , optSkip    :: Int
      , optReverse :: Bool
      , optPrimer  :: String
      , runAction  :: CmdAction
      }
  | CmdSplit {
        optInput    :: FilePath
      , optOutput   :: Maybe FilePath
      , optBarcodes :: FilePath
      , runAction   :: CmdAction
      }
