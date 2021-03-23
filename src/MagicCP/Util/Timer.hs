module MagicCP.Util.Timer where

import Data.IORef       (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import System.Clock
    ( Clock (Realtime)
    , TimeSpec (..)
    , fromNanoSecs
    , getTime
    , toNanoSecs
    )
import System.IO.Unsafe (unsafePerformIO)


totalTime :: IORef Integer
{-# NOINLINE totalTime #-}
totalTime = unsafePerformIO (newIORef 0)

lastStart :: IORef Integer
{-# NOINLINE lastStart #-}
lastStart = unsafePerformIO (newIORef 0)

reset :: IO ()
reset  = writeIORef totalTime 0

start :: IO ()
start = do
  t <- toNanoSecs <$> getTime Realtime
  writeIORef lastStart t

pause :: IO ()
pause = do
  t <- toNanoSecs <$> getTime Realtime
  tstart <- readIORef lastStart
  modifyIORef totalTime (+ (t - tstart))

getTotal :: IO Integer
getTotal = readIORef totalTime

getTotalSecs :: IO Double
getTotalSecs = do
  t <- fromIntegral <$> getTotal
  return $ t / 1000000000
