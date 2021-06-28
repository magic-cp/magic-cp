module MagicCP.Util.Logger(newLogger, logParser, logPrimitives, logSubmission, write) where

import CF                       (ProblemId)
import CF.CFToolWrapper         (Verdict (..))
import Data.IORef               (IORef)
import MagicCP.ParseInputOutput (ParseInputOutput (..), WithTestCases)
import MagicCP.SearchOptions    (WithAbsents, WithOptimizations)
import System.FilePath.Posix    ((</>))

import qualified Data.IORef
import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.IO.Unsafe
import qualified Text.Printf      as Printf

logFile :: IORef FilePath
{-# NOINLINE logFile  #-}
logFile = System.IO.Unsafe.unsafePerformIO (Data.IORef.newIORef "")

write :: String -> IO ()
write s = do
  file <- Data.IORef.readIORef  logFile
  appendFile file s
  appendFile file "\n"

newLogger :: FilePath -> ProblemId -> WithOptimizations -> WithAbsents -> IO ()
newLogger log_root (cId, pId) wOps wAbs = do
  System.Directory.createDirectoryIfMissing True log_root
  time <- words . takeWhile (/= '.') . show <$> Data.Time.Clock.getCurrentTime
  let time' = head time ++ "-" ++ time!!1
      file = log_root </> (time' ++ "-" ++ show cId ++ [pId])
  Data.IORef.writeIORef logFile file
  write time'
  write (show cId ++ [pId])
  write (show wOps)
  write (show wAbs)
  write ""

logParser :: ParseInputOutput b => b -> WithTestCases -> IO ()
logParser hoge wTC = do
  write $ parserName hoge wTC
  write ""

logPrimitives :: String -> IO ()
logPrimitives prims = do
  write prims
  write ""

logSubmission :: String -> Double -> Integer -> Verdict -> IO ()
logSubmission expression time exps verd = do
  write $ "Expression submited: " ++ expression
  write $ Printf.printf "Time: %.3fs" time
  write $ Printf.printf "Expressions tried: %d" exps
  case verd of
    Accepted -> write $ Printf.printf "Verdict: %s" (show verd)
    Rejected subm msg -> write $ Printf.printf "Verdict: %s" (show (Rejected subm (drop 2 (dropWhile (/= ':') msg))))
  write ""

