module MagicCP.Util.Timer where

import Data.IORef   (IORef)
import System.Clock (Clock (..))

import qualified Data.IORef
import qualified System.Clock
import qualified System.IO.Unsafe


totalTime :: IORef Integer
{-# NOINLINE totalTime #-}
totalTime = System.IO.Unsafe.unsafePerformIO (Data.IORef.newIORef 0)

lastStart :: IORef Integer
{-# NOINLINE lastStart #-}
lastStart = System.IO.Unsafe.unsafePerformIO (Data.IORef.newIORef 0)

reset :: IO ()
reset = Data.IORef.writeIORef totalTime 0

start :: IO ()
start = do
  t <- System.Clock.toNanoSecs <$> System.Clock.getTime Realtime
  Data.IORef.writeIORef lastStart t

pause :: IO ()
pause = do
  t <- System.Clock.toNanoSecs <$> System.Clock.getTime Realtime
  tstart <- Data.IORef.readIORef lastStart
  Data.IORef.modifyIORef totalTime (+ (t - tstart))

getTotal :: IO Integer
getTotal = Data.IORef.readIORef totalTime

getTotalSecs :: IO Double
getTotalSecs = do
  t <- fromIntegral <$> getTotal
  return $ t / 1000000000
