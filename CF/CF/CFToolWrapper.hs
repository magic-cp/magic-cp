{-# LANGUAGE RecordWildCards #-}
module CF.CFToolWrapper(getInputOutput, testSolution) where

import Control.Monad( liftM2 )

import Data.Char( toLower )
import Data.List( partition, isPrefixOf, sort )

import System.Directory( createDirectoryIfMissing, removePathForcibly
                       , renameDirectory ,listDirectory, removeDirectory
                       )
import System.FilePath.Posix( (</>) )
import System.Process( readProcess )
import System.Posix.Directory( changeWorkingDirectory )

import CF.CFConfig

data Verdict = Accepted | Rejected deriving Show

problemIdToStrings :: (Int, Char) -> (String, String)
problemIdToStrings (cIdInt, pIdChar) = (show cIdInt, [toLower pIdChar])

getInputOutput :: CFConfig -> (Int, Char) -> IO [(String, String)]
getInputOutput CFConfig{..} problemId = do
  createDirectoryIfMissing True cfparse_dir
  changeWorkingDirectory cfparse_dir
  _ <- readProcess (cftool_path </> "cf") ["parse", cId, pId] ""

  dir <- listDirectory $ cfparse_dir </> cId </> pId
  let (ins, outs) = partition ("in" `isPrefixOf`) dir
  changeWorkingDirectory project_root
  liftM2 zip (mapM readFile' $ sort ins) (mapM readFile' $ sort outs)
  where
    (cId, pId) = problemIdToStrings problemId
    readFile' :: FilePath -> IO String
    readFile' fp = readFile $ cfparse_dir </> cId </> pId </> fp

testSolution :: CFConfig -> (Int, Char) -> IO Verdict
testSolution CFConfig{..} problemId = do
  let (cId, pId) = problemIdToStrings problemId

  changeWorkingDirectory $ cfparse_dir </> cId </> pId
  output <- readProcess (cftool_path </> "cf") ["test"] ""
  print output

  changeWorkingDirectory project_root
  return Accepted
