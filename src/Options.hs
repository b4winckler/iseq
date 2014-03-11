module Options (
    IseqOptions(..)
  , Command(..)
  , Alignment(..)
  ) where


data IseqOptions = IseqOptions {
      optVerbose :: Bool
    , optOutput  :: Maybe FilePath
    , optCommand :: Command
    }

type CmdAction = IseqOptions -> IO ()

data Command =
    CmdAlign {
        optAlignment :: Alignment
      , runAction    :: CmdAction
      }
  | CmdMerge { runAction :: CmdAction }
  | CmdScan {
        optInput  :: FilePath
      , optShift  :: Int
      , optErrors :: Int
      , optSkip   :: Int
      , optPrimer :: String
      , runAction :: IseqOptions -> IO ()
      }

data Alignment =
    GlobalAlignment
  | LocalAlignment
  deriving (Show, Eq)

