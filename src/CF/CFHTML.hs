{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module CF.CFHTML( getTestCases ) where

import CF.CFConfig           (CFConfig (..))
import System.FilePath.Posix ((</>))

import qualified Data.List
import qualified Data.List.Split
import qualified System.Process

getTestCases :: CFConfig -> Int -> Int -> IO [Maybe (String, String)]
getTestCases CFConfig{..} cId subId = do
    let cfSubmissionUrl = "https://codeforces.com/contest/" <> show cId <> "/submission/" <> show subId
    io <- System.Process.readProcess "python" [project_root </> "scripts/sel.py", cfSubmissionUrl] ""

    let splitTestCase = Data.List.Split.splitOn ["##ENDOFCASE##"]
    let splitInputOutput = span ("##ENDOFINPUT##" /=)
    let mkPair (unlines -> i, unlines . drop 1 -> o) =
            if "..." `Data.List.isSuffixOf` i || "..." `Data.List.isSuffixOf` o
            then Nothing
            else Just (i, o)

    return $ map (mkPair . splitInputOutput) $ splitTestCase $ lines io
