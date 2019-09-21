module MagicCP where

import MagicHaskeller
import Data.IORef

test :: IO ()
test = do
  pg <- readIORef refmemodeb
  let cmn = extractCommon pg

  return ()
