{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}


module MagicCP where

import qualified Language.Haskell.TH as TH

import           Control.Monad                  ( when
                                                , mplus
                                                , liftM2
                                                )
import           Control.Exception
import           Control.Concurrent             ( myThreadId
                                                , forkIO
                                                , killThread
                                                , threadDelay
                                                , throwTo
                                                )
import Data.Array( elems )
import Data.Char( toLower )
import Data.Generics(everywhere, mkT, Data)
import Data.Time.Clock( getCurrentTime )
import           Data.Maybe                     ( fromJust )
import Debug.Trace
import System.FilePath.Posix( (</>) )
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                )
import           System.Process                 ( callCommand )

import CF
import CF.CFConfig
import CF.CFToolWrapper
import CF.CFHTML

import MagicCP.Util.Memory
import MagicCP.ParseInputOutput

import MagicHaskeller hiding ( TH(..) )
import MagicHaskeller.ProgGenSF
import MagicHaskeller.ProgramGenerator
import MagicHaskeller.LibTH( initializeTest, reallyalltest )
import MagicHaskeller.LibTHDefinitions
import MagicHaskeller.TimeOut( maybeWithTO )

import MagicCP.ParserDefinitions

checkInitialized :: IO ()
checkInitialized = do
  pg <- extractCommon <$> getPG
  when (null $ elems (vl pg) ++ elems (pvl pg)) $ putStrLn "ProgramGenerator not initialized."

generateFile :: CFConfig -> ProblemId -> TH.Exp -> IO ()
generateFile CFConfig{..} (cId, pIndex) e = do
  time <- getCurrentTime
  program <- runQ $ concat <$> sequence
    [ allDeclarations
    , parser1Def
    , (:[]) <$> funD (mkName "solve") [clause [] (normalB $ return e) []]
    , [d| main = getContents >>= \c -> putStrLn (uncurry solve (parser1 c)) |]
    ]
  writeFile fileName ("-- " <> show time <> "\n" <> pprintUC program)
  where
    fileName = cfparse_dir </> show cId </> [toLower pIndex] </> (toLower pIndex:".hs")

pprintUC :: (Ppr a, Data a) => a -> String
pprintUC =  pprint . everywhere (mkT unqCons)
unqCons :: TH.Name -> TH.Name
unqCons n = mkName (nameBase n)

solveWithAllParsers :: IO (Maybe Exp)
solveWithAllParsers = do
  let l = [ solveWithLimits (solvev0 :: ((Int -> [Int] -> String) -> ProblemId -> IO Exp)) (1030, 'a')
          , solveWithLimits (solvev0 :: ((String -> String) -> ProblemId -> IO Exp)) (1030, 'a')
          ]
  solveUntilJust l
  where
    solveUntilJust :: [IO (Maybe Exp)] -> IO (Maybe Exp)
    solveUntilJust [] = return Nothing
    solveUntilJust (f:l) = do
      me <- f
      case me of
        Just _ -> return me
        Nothing -> solveUntilJust l


solveWithLimits :: ParseInputOutput b => (b -> ProblemId -> IO Exp) -> ProblemId -> IO (Maybe Exp)
solveWithLimits solve pId = do
  tid <- myThreadId
  let timeout = 60*60*4
      memoPerc = 90
  bracket
    ( forkIO $ checkLimits tid timeout memoPerc )
    killThread
    ( \_ -> Just <$> solve undefined pId )
    `catch` \(e :: SomeException) -> return Nothing

  where
  checkLimits tid tout memLimit = do
    memusage <- getMemoUsage
    putStrLn $ "checking limits: " ++ show tout ++ "  " ++ show memusage
    if (memusage > memLimit || tout < 0)
       then killThread tid
       else do
        threadDelay 5000000
        checkLimits tid (tout - 5) memLimit


-- solvev0 (undefined :: (Int -> [Int] -> String)) (1030, 'a')
solvev0 :: forall b . (Typeable b, ParseInputOutput b) => b -> ProblemId -> IO Exp
solvev0 hoge pId@(cId, _) = do
  checkInitialized
  cfg <- getCFConfig
  putStrLn (wut hoge)

  putStrLn "Parsing problem"
  ios <- getInputOutput cfg pId
  --let pred = fromJust $ getPredicate 0 ios :: ((Int -> [Int] -> String) -> Bool)
  let pred = fromJust $ (getPredicate 0 ios :: Maybe (b -> Bool))

  putStrLn "Starting search"
  md <- getPG
  --let md = reallyalltest::ProgGenSF
  let et = everything md True
      mpto = timeout $ opt $ extractCommon md
  f cfg mpto pred (concat et)
  where
    f :: (ParseInputOutput a) => CFConfig -> Maybe Int -> (a -> Bool) -> [(Exp, a)] -> IO Exp
    f cfg mpto pred ((e, a):ts) = do
      result <- maybeWithTO undefined mpto (return (pred a))
      case result of
        Just True -> do
          generateFile cfg pId e
          testVerd <- testSolution cfg pId
          case testVerd of
            Accepted -> do
              callCommand "beep -f 800 -l 200 -d 200 -n -f 800 -l 300"
              putStrLn "Submitting to codeforces"
              submitVerd <- submitSolution cfg pId
              case submitVerd of
                Accepted -> do
                  callCommand "beep -f 800 -l 200 -d 200 -n -f 1200 -l 300"
                  putStrLn $ "Solution accepted in codeforces:\n" <>
                    pprintUC e
                  return e
                Rejected subm msg -> do
                  callCommand "beep -f 800 -l 200 -d 200 -n -f 600 -l 300"
                  putStrLn $ "sumbission #" <> show subm <> " failed with: " <>
                    drop 2 (dropWhile (/= ':') msg)
                  putStrLn "add new test cases in folder (if you want) and press F"
                  getChar
                  ios <- getCurrentInputOutput cfg pId
                  let pred' = fromJust $ getPredicate 0 ios
                  f cfg mpto pred' ts

            Rejected{} -> do
              putStrLn "Failed Sample Tests"
              f cfg mpto pred ts
        Just False -> do
          --tid <- myThreadId
          --trace (show tid) $ return ()
          f cfg mpto pred ts
        Nothing    -> hPutStrLn stderr ("timeout on "++pprintUC e) >> f cfg mpto pred ts
