module MagicCP.Util.Memory where

import qualified System.Process

getMemoUsage :: IO Float
getMemoUsage = (100 -) <$> getFreeMemo
  where
    getFreeMemo :: IO Float
    getFreeMemo = do
      out <- System.Process.readProcess "free" [] ""
      let x = (!!1) . map words . lines $ out
          total = read (x!!1)
          free = read (last x)
          percent = free*100/total
      return percent
