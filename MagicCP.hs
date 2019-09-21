{-# LANGUAGE TemplateHaskell #-}
module MagicCP where

import Language.Haskell.TH as TH

import CF
import CF.CFConfig

import MagicHaskeller
import MagicHaskeller.LibTH(initializeTest)

solvev0 :: ProblemId -> IO (Maybe TH.Exp)
solvev0 pId = do
  initializeTest
  cfg <- getCFConfig
  p <- getPredicate cfg pId
  findDo (\e _ -> return (Just e)) True p
