module CF.CFToolWrapper(getInputOutput, submitSolution) where

import Control.Monad( liftM2 )

import Data.Char( toLower )
import Data.List( partition, isPrefixOf, sort )

import System.Directory( removePathForcibly, renameDirectory ,listDirectory )
import System.FilePath.Posix( (</>) )
import System.Process( readProcess )
import System.Posix.Directory( getWorkingDirectory, changeWorkingDirectory )

data Verdict = Accepted | Rejected deriving Show

problemIdToStrings :: (Int, Char) -> (String, String)
problemIdToStrings (cIdInt, pIdChar) = (show cIdInt, [toLower pIdChar])

getInputOutput :: (Int, Char) -> IO [(String, String)]
getInputOutput problemId = do
  _ <- readProcess "./cf-tool/cf" ["parse", cId, pId] ""

  removePathForcibly $ "./inputs" </> cId </> pId
  renameDirectory ("." </> cId) $ "./inputs" </> cId

  dir <- listDirectory $ "./inputs" </> cId </> pId
  let (ins, outs) = partition ("in" `isPrefixOf`) dir
  liftM2 zip (mapM readFile' $ sort ins) (mapM readFile' $ sort outs)
  where
    (cId, pId) = problemIdToStrings problemId
    readFile' :: FilePath -> IO String
    readFile' fp = readFile $ "./inputs" </> cId </> pId </> fp

submitSolution :: (Int, Char) -> FilePath -> IO Verdict
submitSolution problemId fp = do
  let (cId, pId) = problemIdToStrings problemId

  changeWorkingDirectory $ "./inputs" </> cId </> pId
  output <- readProcess "../../../cf-tool/cf" ["parse", cId, pId] ""
  print output

  changeWorkingDirectory "../../.."
  return Accepted
