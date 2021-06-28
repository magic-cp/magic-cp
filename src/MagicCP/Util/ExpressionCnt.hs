module MagicCP.Util.ExpressionCnt where

import Data.IORef (IORef)

import qualified Data.IORef
import qualified System.IO.Unsafe


totalExps :: IORef Integer
{-# NOINLINE totalExps  #-}
totalExps = System.IO.Unsafe.unsafePerformIO (Data.IORef.newIORef 0)

cntExp :: IO ()
cntExp = Data.IORef.modifyIORef totalExps (+ 1)

reset :: IO ()
reset  = Data.IORef.writeIORef totalExps 0

getTotalExps :: IO Integer
getTotalExps = Data.IORef.readIORef totalExps
