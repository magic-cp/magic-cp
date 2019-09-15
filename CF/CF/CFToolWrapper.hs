module CF.CFToolWrapper where

import           Control.Monad                  ( liftM2 )

import           Data.Char                      ( toLower )
import           Data.List                      ( partition
                                                , isPrefixOf
                                                , sort
                                                )

import           System.Directory               ( removePathForcibly
                                                , renameDirectory
                                                , listDirectory
                                                )
import           System.FilePath.Posix          ( (</>) )
import           System.Process                 ( readProcess )


getInputOutput :: (Int, Char) -> IO [(String, String)]
getInputOutput (cIdInt, pIdChar) = do
  _ <- readProcess "./cf-tool/cf" ["parse", cId, pId] ""

  removePathForcibly $ "./inputs" </> cId </> pId
  renameDirectory ("." </> cId) $ "./inputs" </> cId

  dir <- listDirectory $ "./inputs" </> cId </> pId
  let (ins, outs) = partition ("in" `isPrefixOf`) dir
  liftM2 zip (mapM readFile' $ sort ins) (mapM readFile' $ sort outs)
  where
    cId :: String
    cId = show cIdInt
    pId :: String
    pId = [toLower pIdChar]
    readFile' :: FilePath -> IO String
    readFile' fp = readFile $ "./inputs" </> cId </> pId </> fp
