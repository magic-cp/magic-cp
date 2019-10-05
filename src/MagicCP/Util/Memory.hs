module MagicCP.Util.Memory where

import System.Process( readProcess )

getMemoUsage :: IO Float
getMemoUsage = (100 -) <$> getFreeMemo

getFreeMemo :: IO Float
getFreeMemo = do
  out <- readProcess "free" [] ""
  let x = (!!1) . map words . lines $ out
      total = read (x!!1)
      free = read (last x)
      percent = free*100/total
  return percent
