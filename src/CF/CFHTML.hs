{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module CF.CFHTML( getLastTestCase ) where

import CF.CFConfig           (CFConfig (..))
import System.FilePath.Posix ((</>))

import qualified Data.List
import qualified System.Process

getLastTestCase :: CFConfig -> Int -> Int -> IO (Maybe (String, String))
getLastTestCase CFConfig{..} cId subId = do
  let cId' = show cId
      subId' = show subId
      url = "https://codeforces.com/contest/" <> cId' <> "/submission/" <> subId'
  io <- System.Process.readProcess "python" [project_root </> "scripts/sel.py", url] ""
  let (i, o) = span ("##ENDOFINPUT##" /=) $ lines io
      lastIn = unlines i
      lastOut = unlines $ drop 1 o
  if "..." `Data.List.isSuffixOf` lastIn || "..." `Data.List.isSuffixOf` lastOut
     then return Nothing
     else return $ Just (lastIn, lastOut)

