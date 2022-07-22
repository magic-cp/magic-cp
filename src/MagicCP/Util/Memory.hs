module MagicCP.Util.Memory (MemoryUsage(..), getMemoUsage) where

import qualified System.Posix.Process as Process
import qualified System.Process

data MemoryUsage = Exceeded Double | NotExceeded Double

instance Show MemoryUsage where
  show (Exceeded pct) = show pct
  show (NotExceeded pct) = show pct

getMemoUsage :: IO MemoryUsage
getMemoUsage = do
  pid <- show <$> Process.getProcessID
  out <- System.Process.readProcess "ps" ["-p", pid, "-o", "pmem"] ""
  case lines out of
    ["%MEM", pctStr] -> let pct = read pctStr :: Double in return $ if pct >= 80 then Exceeded pct else NotExceeded pct
    _ -> do
      putStrLn "Parse error of memory usage"
      putStrLn out
      return $ NotExceeded 0
