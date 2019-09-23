{-# LANGUAGE TemplateHaskell #-}
module MagicCP where

import Language.Haskell.TH as TH

import Data.Array


import CF
import CF.CFConfig

import MagicHaskeller
import MagicHaskeller.ProgramGenerator
import MagicHaskeller.LibTH( initializeTest )
import Control.Monad( when )

checkInitialized :: IO ()
checkInitialized = do
  pg <- extractCommon <$> getPG
  when (null $ elems (vl pg) ++ elems (pvl pg)) $ putStrLn "ProgramGenerator not initialized."


solvev0 :: ProblemId -> IO (Maybe TH.Exp)
solvev0 pId = do
  checkInitialized
  cfg <- getCFConfig
  p <- getPredicate cfg pId
  findDo (\e _ -> return (Just e)) True p
