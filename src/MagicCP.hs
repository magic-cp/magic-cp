{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module MagicCP where

import qualified Language.Haskell.TH as TH

import Control.Monad( when )
import Data.Array( elems )
import Data.Char( toLower )
import Data.Generics(everywhere, mkT, Data)
import Data.Time.Clock( getCurrentTime )
import Debug.Trace
import System.FilePath.Posix( (</>) )
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                )

import CF
import CF.CFConfig
import CF.CFToolWrapper
import CF.CFHTML

import MagicHaskeller hiding ( TH(..) )
import MagicHaskeller.ProgramGenerator
import MagicHaskeller.LibTH( initializeTest )
import MagicHaskeller.LibTHDefinitions
import MagicHaskeller.TimeOut( maybeWithTO )

checkInitialized :: IO ()
checkInitialized = do
  pg <- extractCommon <$> getPG
  when (null $ elems (vl pg) ++ elems (pvl pg)) $ putStrLn "ProgramGenerator not initialized."

generateFile :: CFConfig -> ProblemId -> TH.Exp -> IO ()
generateFile CFConfig{..} (cId, pIndex) e = do
  time <- getCurrentTime
  program <- runQ $ concat <$> sequence
    [ allDeclarations
    , (:[]) <$> funD (mkName "solve") [clause [] (normalB $ return e) []]
    , [d| main = getContents >>= \c -> putStrLn (solve c) |]
    ]
  writeFile fileName ("-- " <> show time <> "\n" <> pprintUC program)
  where
    fileName = cfparse_dir </> show cId </> [toLower pIndex] </> (toLower pIndex:".hs")

pprintUC :: (Ppr a, Data a) => a -> String
pprintUC =  pprint . everywhere (mkT unqCons)
unqCons :: TH.Name -> TH.Name
unqCons n = mkName (nameBase n)


solvev0 :: ProblemId -> IO Exp
solvev0 pId@(cId, _) = do
  checkInitialized
  cfg <- getCFConfig
  pred <- getPredicate cfg pId

  et <- getEverything True
  md <- getPG
  let mpto = timeout $ opt $ extractCommon md
  f cfg mpto pred (concat et)
  where
    f cfg mpto pred ((e, a):ts) = do
      result <- maybeWithTO seq mpto (return (pred a))
      case result of
        Just True -> do
          generateFile cfg pId e
          testVerd <- testSolution cfg pId
          case testVerd of
            Accepted -> do
              putStrLn "Submitting to codeforces"
              submitVerd <- submitSolution cfg pId
              case submitVerd of
                Accepted -> do
                  putStrLn $ "Solution accepted in codeforces:\n" <>
                    pprintUC e
                  return e
                Rejected subm msg -> do
                  putStrLn $ "sumbission #" <> show subm <> " failed with: " <>
                    drop 2 (dropWhile (/= ':') msg)
                  mtc <- getLastTestCase cId subm
                  case mtc of
                    Just io -> f cfg mpto (extendPredicate pred io) ts
                    Nothing -> f cfg mpto pred ts

            Rejected{} -> do
              putStrLn "Failed Sample Tests"
              f cfg mpto pred ts
        Just False -> f cfg mpto pred ts
        Nothing    -> hPutStrLn stderr ("timeout on "++pprintUC e) >> f cfg mpto pred ts
