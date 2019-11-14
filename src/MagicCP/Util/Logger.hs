module MagicCP.Util.Logger(newLogger, logParser, logPrimitives, logSubmission) where

import MagicCP.SearchOptions
import MagicCP.ParseInputOutput
import CF
import CF.CFToolWrapper

import           Data.IORef                     ( IORef, newIORef, writeIORef
                                                , readIORef, modifyIORef )
import           System.IO.Unsafe               ( unsafePerformIO )
import           System.FilePath.Posix          ( (</>) )
import           System.Directory               ( createDirectoryIfMissing )

import           Data.Time.Clock                ( getCurrentTime )
import           Text.Printf                    ( printf )

logFile :: IORef FilePath
{-# NOINLINE logFile  #-}
logFile = unsafePerformIO (newIORef "")

write :: String -> IO ()
write s = do
  file <- readIORef  logFile
  appendFile file s
  appendFile file "\n"

newLogger :: FilePath -> ProblemId -> WithOptimizations -> WithAbsents -> IO ()
newLogger log_root (cId, pId) wOps wAbs = do
  createDirectoryIfMissing True log_root
  time <- words . takeWhile (/= '.') . show <$> getCurrentTime
  let time' = head time ++ "-" ++ time!!1
      file = log_root </> (time' ++ "-" ++ show cId ++ [pId])
  writeIORef logFile file
  write time'
  write (show cId ++ [pId])
  write (show wOps)
  write (show wAbs)
  write ""

logParser :: ParseInputOutput b => b -> IO ()
logParser hoge = do
  write $ parserName hoge
  write ""

logPrimitives :: String -> IO ()
logPrimitives prims = do
  write prims
  write ""

logSubmission :: String -> Double -> Integer -> Verdict -> IO ()
logSubmission exp time exps verd = do
  write $ "Expression submited: " ++ exp
  write $ printf "Time: %.3fs" time
  write $ printf "Expressions tried: %d" exps
  case verd of
    Accepted -> write $ printf "Verdict: %s" (show verd)
    Rejected subm msg -> write $ printf "Verdict: %s" (show (Rejected subm (drop 2 (dropWhile (/= ':') msg))))
  write ""

