{-# LANGUAGE RecordWildCards #-}
module CF.CFToolWrapper(getInputOutput, Verdict(..), testSolution, submitSolution) where

import CF.CFConfig           (CFConfig (..))
import System.FilePath.Posix ((</>))

import qualified Control.Monad
import qualified Data.Char
import qualified Data.List
import qualified System.Directory
import qualified System.Process

data Verdict = Accepted | Rejected Int String deriving Show

problemIdToStrings :: (Int, Char) -> (String, String)
problemIdToStrings (cIdInt, pIdChar) = (show cIdInt, [Data.Char.toLower pIdChar])

getInputOutput :: CFConfig -> (Int, Char) -> IO [(String, String)]
getInputOutput CFConfig{..} problemId = do
  System.Directory.createDirectoryIfMissing True cfparse_dir
  System.Directory.withCurrentDirectory cfparse_dir $ do
    System.Directory.removePathForcibly $ cfparse_dir </> cId </> pId
    _ <- System.Process.readProcess (cftool_path </> "cf") ["parse", cId, pId] ""
    return ()
  getCurrentInputOutput
  where
    (cId, pId) = problemIdToStrings problemId

    getCurrentInputOutput = do
      dir <- System.Directory.listDirectory $ cfparse_dir </> cId </> pId
      let ins = filter ("in" `Data.List.isPrefixOf`) dir
          outs = filter ("ans" `Data.List.isPrefixOf`) dir
      Control.Monad.liftM2 zip (mapM readFile' $ Data.List.sort ins) (mapM readFile' $ Data.List.sort outs)
      where
        readFile' :: FilePath -> IO String
        readFile' fname = readFile $ cfparse_dir </> cId </> pId </> fname

testSolution :: CFConfig -> (Int, Char) -> IO Verdict
testSolution CFConfig{..} problemId = do
  let (cId, pId) = problemIdToStrings problemId

  output <- System.Directory.withCurrentDirectory (cfparse_dir </> cId </> pId) $ do
    System.Process.readProcess (cftool_path </> "cf") ["test"] ""
  if "Failed" `Data.List.isInfixOf` output
     then return $ Rejected 0 output
     else return Accepted


submitSolution :: CFConfig -> (Int, Char) -> IO Verdict
submitSolution CFConfig{..} problemId = do
  let (cId, pId) = problemIdToStrings problemId

  output <- System.Directory.withCurrentDirectory (cfparse_dir </> cId </> pId) $ do
    System.Process.readProcess (cftool_path </> "cf") ["submit"] ""
  let submission = read
                    $ drop 2 $ dropWhile (/= ':') $ last
                    $ filter ("#:" `Data.List.isInfixOf`) $ lines output
  putStrLn output
  let status = last $ filter ("status:" `Data.List.isInfixOf`) $ lines output
  if "Accepted" `Data.List.isInfixOf` status
     then return Accepted
     else return $ Rejected submission status
