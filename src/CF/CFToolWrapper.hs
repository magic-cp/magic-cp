{-# LANGUAGE RecordWildCards #-}
module CF.CFToolWrapper(getInputOutput, getCurrentInputOutput, Verdict(..), testSolution, submitSolution) where

import Control.Monad( liftM2 )

import Data.Char( toLower )
import Data.List( partition, isPrefixOf, sort, isInfixOf )

import System.Directory( createDirectoryIfMissing, removePathForcibly
                       , renameDirectory ,listDirectory
                       )
import System.FilePath.Posix( (</>) )
import System.Process( readProcess )
import System.Posix.Directory( changeWorkingDirectory )

import CF.CFConfig

data Verdict = Accepted | Rejected Int String deriving Show

problemIdToStrings :: (Int, Char) -> (String, String)
problemIdToStrings (cIdInt, pIdChar) = (show cIdInt, [toLower pIdChar])

getInputOutput :: CFConfig -> (Int, Char) -> IO [(String, String)]
getInputOutput cfconf@CFConfig{..} problemId = do
  createDirectoryIfMissing True cfparse_dir
  changeWorkingDirectory cfparse_dir
  removePathForcibly $ cfparse_dir </> cId </> pId
  _ <- readProcess (cftool_path </> "cf") ["parse", cId, pId] ""
  changeWorkingDirectory project_root
  getCurrentInputOutput cfconf problemId
  where
    (cId, pId) = problemIdToStrings problemId

getCurrentInputOutput :: CFConfig -> (Int, Char) -> IO [(String, String)]
getCurrentInputOutput CFConfig{..} problemId = do
  dir <- listDirectory $ cfparse_dir </> cId </> pId
  let ins = filter ("in" `isPrefixOf`) dir
      outs = filter ("ans" `isPrefixOf`) dir
  changeWorkingDirectory project_root
  liftM2 zip (mapM readFile' $ sort ins) (mapM readFile' $ sort outs)
  where
    (cId, pId) = problemIdToStrings problemId
    readFile' :: FilePath -> IO String
    readFile' fname = readFile $ cfparse_dir </> cId </> pId </> fname

testSolution :: CFConfig -> (Int, Char) -> IO Verdict
testSolution CFConfig{..} problemId = do
  let (cId, pId) = problemIdToStrings problemId

  changeWorkingDirectory $ cfparse_dir </> cId </> pId
  output <- readProcess (cftool_path </> "cf") ["test"] ""
  changeWorkingDirectory project_root
  if "Failed" `isInfixOf` output
     then return $ Rejected 0 output
     else return Accepted


submitSolution :: CFConfig -> (Int, Char) -> IO Verdict
submitSolution CFConfig{..} problemId = do
  let (cId, pId) = problemIdToStrings problemId

  changeWorkingDirectory $ cfparse_dir </> cId </> pId
  output <- readProcess (cftool_path </> "cf") ["submit"] ""
  changeWorkingDirectory project_root
  let submission = read $ drop 2 $ dropWhile (/= ':') $ last $ filter ("#:" `isInfixOf`) $ lines output
  let status = last $ filter ("status:" `isInfixOf`) $ lines output
  if "Accepted" `isInfixOf` status
     then return Accepted
     else return $ Rejected submission status
