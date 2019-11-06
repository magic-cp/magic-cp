module MagicCP.Util.ExpressionCnt where

import           Data.IORef                     ( IORef, newIORef, writeIORef
                                                , readIORef, modifyIORef )
import           System.IO.Unsafe               ( unsafePerformIO )


totalExps :: IORef Integer
{-# NOINLINE totalExps  #-}
totalExps = unsafePerformIO (newIORef 0)

cntExp :: IO ()
cntExp = modifyIORef totalExps (+ 1)

reset :: IO ()
reset  = writeIORef totalExps 0

getTotalExps :: IO Integer
getTotalExps = readIORef totalExps
