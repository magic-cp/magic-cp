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
import Data.List
import Data.Generics(everywhere, mkT, Data)
import Data.Time.Clock( getCurrentTime )
import           Data.Maybe                     ( fromJust )
import Debug.Trace
import System.FilePath.Posix( (</>) )
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                )
import           System.Process                 ( callCommand )
import Text.Printf

import Unsafe.Coerce(unsafeCoerce)

import CF
import CF.CFConfig
import CF.CFToolWrapper
import CF.CFHTML

import MagicCP.SearchOptions
import MagicCP.Util.Memory
import qualified MagicCP.Util.Logger as Logger
import qualified MagicCP.Util.Timer as Timer
import qualified MagicCP.Util.ExpressionCnt as ECnt
import MagicCP.ParseInputOutput

import MagicHaskeller hiding ( TH(..) )
import MagicHaskeller.ProgGen
import MagicHaskeller.ProgramGenerator
import MagicHaskeller.LibTH( mkPGWithDefaultsOpts, mkPGWithDefaults )
import MagicHaskeller.LibTHDefinitions
import MagicHaskeller.TimeOut( maybeWithTO2 )


import MagicCP.ParserDefinitions

checkInitialized :: IO ()
checkInitialized = do
  pg <- extractCommon <$> getPG
  when (null $ elems (vl pg) ++ elems (pvl pg)) $ putStrLn "ProgramGenerator not initialized."

generateFile :: ParseInputOutput b => CFConfig -> ProblemId -> b -> WithTestCases -> TH.Exp -> IO ()
generateFile CFConfig{..} (cId, pIndex) hoge wTC e = do
  let parserDecs = parserDeclarations hoge
      mainDecs = mainParserDeclarations hoge wTC
  time <- getCurrentTime
  program <- runQ $ concat <$> sequence
    [ allDeclarations
    , parserDecs
    , (:[]) <$> funD (mkName "solve") [clause [] (normalB $ return e) []]
    , mainDecs
    ]
  writeFile fileName $
    "-- " <> show time <> "\n" <>
    "import Data.Maybe\n" <>
    "import Control.Monad\n" <>
    "import Text.Read\n" <>
    pprintUC program
  where
    fileName = cfparse_dir </> show cId </> [toLower pIndex] </> (toLower pIndex:".hs")

pprintUC :: (Ppr a, Data a) => a -> String
pprintUC =  pprint . everywhere (mkT unqCons)
unqCons :: TH.Name -> TH.Name
unqCons n = mkName (nameBase n)

solveWithAllParsers
  :: WithOptimizations
  -> WithAbsents
  -> CFConfig
  -> [PrimitiveWithOpt]
  -> ProblemId
  -> IO (Maybe Exp)
solveWithAllParsers wOps wAbs cfg lib pId = do
  let l =
        [ solveWithLimits (solvev0 wOps wAbs WithoutTestCases cfg lib
            (undefined :: Int -> Int -> Int -> String)) pId
        , solveWithLimits (solvev0 wOps wAbs WithTestCases cfg lib
            (undefined :: Int -> Int -> Int -> Int -> Int)) pId
        , solveWithLimits (solvev0 wOps wAbs WithTestCases cfg lib
            (undefined :: Int -> Int -> String)) pId
        , solveWithLimits (solvev0 wOps wAbs WithoutTestCases cfg lib
            (undefined :: [Int] -> String)) pId
        , solveWithLimits (solvev0 wOps wAbs WithoutTestCases cfg lib
            (undefined :: Int -> [Int] -> String)) pId
        --, solveWithLimits (solvev0 wOps wAbs WithoutTestCases cfg lib (undefined :: Int -> String)) pId
        , solveWithLimits (solvev0 wOps wAbs WithoutTestCases cfg lib
            (undefined :: String -> String)) pId
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

solveWithLimits :: (ProblemId -> IO Exp) -> ProblemId -> IO (Maybe Exp)
solveWithLimits solve pId = do
  tid <- myThreadId
  let timeout = 60*12
      memoPerc = 80
  bracket
    ( forkIO $ checkLimits tid timeout memoPerc )
    killThread
    ( \_ -> Just <$> solve pId )
    --`catch` \(ErrorCall _) -> do
    `catch` \(SomeException _) -> do
      callCommand "beep -f 800 -l 30 -d 200 -n -f 600 -l 20 -n -f 300 -l 20 -n -f 100 -l 50"
      --putStrLn s
      return Nothing
  where
  checkLimits tid tout memLimit = do
    memusage <- getMemoUsage
    when (tout `mod` 300 == 0) $ putStrLn $ "checking limits: " ++ show tout ++ "  " ++ show memusage
    if memusage > memLimit || tout < 0
       then killThread tid
       else do
        threadDelay 5000000
        checkLimits tid (tout - 5) memLimit


-- solvev0 (undefined :: (Int -> [Int] -> String)) (1030, 'a')
solvev0
  :: forall b . (Typeable b, ParseInputOutput b)
  => WithOptimizations
  -> WithAbsents
  -> WithTestCases
  -> CFConfig
  -> [PrimitiveWithOpt]
  -> b
  -> ProblemId
  -> IO Exp
solvev0 wOps wAbs wTC cfg customLibrary hoge pId@(cId, _) = do
  putStrLn "Parsing problem"
  ios <- getInputOutput cfg pId
  let pred = fromJust' (getPredicate wTC 0 ios :: Maybe (b -> Bool))
      custom = getConstantPrimitives (typeOf hoge) (concatMap (words . snd) ios)
      (md, prims) = if wOps == WithOptimizations
              then let (md', lst) = mkPGWithDefaultsOpts $
                        customLibrary ++ zip custom (repeat [])
                    in (md', concatMap (\(prim, ops) ->
                          pprintUC prim ++ " " ++ show ops ++ "\n") lst)
              else let (md', lst) = mkPGWithDefaults $
                        map fst customLibrary ++ custom
                    in (md', concatMap (\prim -> pprintUC prim ++ "\n") lst)
  pred `seq` return ()

  Logger.newLogger (log_root cfg) pId wOps wAbs
  Logger.logParser hoge wTC
  Logger.logPrimitives prims

  putStrLn "Starting search!"

  Timer.reset
  Timer.start
  ECnt.reset
  let et = everything md (wAbs == WithAbsents)
      mpto = timeout $ opt $ extractCommon md
  f cfg mpto pred (concat et)
  where
    f :: (ParseInputOutput a)
      => CFConfig
      -> Maybe Int
      -> (a -> Bool)
      -> [(Exp, a)]
      -> IO Exp
    f cfg mpto pred ((e, a):ts) = do
      ECnt.cntExp
      es <- ECnt.getTotalExps
      when (es `mod` 1000 == 0) $ putStrLn (pprintUC e)
      result <- maybeWithTO2 mpto (pred a)
      case result of
        Just True -> do
          trace "found solution to predicate" $ return ()
          Timer.pause
          generateFile cfg pId hoge wTC e
          testVerd <- testSolution cfg pId
          case testVerd of
            Accepted -> do
              submitBeep
              secs <- Timer.getTotalSecs
              es <- ECnt.getTotalExps
              putStrLn $ printf "Submitting to codeforces (%.3fs, %d exps)" secs es
              submitVerd <- submitSolution cfg pId
              Logger.logSubmission (pprintUC e) secs es submitVerd
              case submitVerd of
                Accepted -> do
                  acceptedBeep
                  putStrLn $ "Solution accepted in codeforces:\n" <>
                    pprintUC e
                  secs <- Timer.getTotalSecs
                  putStrLn $ printf "Time: %.3fs" secs
                  es <- ECnt.getTotalExps
                  putStrLn $ printf "Expressions tried: %d" es
                  return e
                Rejected subm msg -> do
                  rejectedBeep
                  putStrLn $ "sumbission #" <> show subm <> " failed with: " <>
                    drop 2 (dropWhile (/= ':') msg)

                  mtc <- getLastTestCase2 cfg cId subm
                  case mtc of
                    Just io -> do
                      putStrLn "Got new test case"
                      Timer.start
                      f cfg mpto (fromJust $ extendPredicate wTC 0 pred io) ts
                    Nothing -> do
                      putStrLn "Couldn't get new test case"
                      Timer.start
                      f cfg mpto pred ts
            Rejected{} -> do
              putStrLn "Failed Sample Tests"
              Timer.start
              f cfg mpto pred ts
        Just False ->
          f cfg mpto pred ts
        Nothing ->
          --hPutStrLn stderr ("timeout on "++pprintUC e)
          f cfg mpto pred ts
    submitBeep = callCommand "beep -f 800 -l 20 -d 200 -n -f 800 -l 30"
    acceptedBeep = callCommand "beep -f 800 -l 20 -d 200 -n -f 1200 -l 30"
    rejectedBeep = callCommand "beep -f 800 -l 20 -d 200 -n -f 600 -l 30"
    fromJust' (Just x) = x
    fromJust' _ = error $ "Wrong parser: " ++ parserName hoge wTC


getConstantPrimitives :: TypeRep -> [String] -> [Primitive]
getConstantPrimitives t ss =
  let a@(cons, args) = splitTyConApp t
      b@(conss, argss) = splitTyConApp (typeRep "hoge")
   in if a == b
         then trace (show ss') $ map buildPrim ss'
         else case args of
                [] -> []
                [ta1] -> getConstantPrimitives ta1 ss
                [_, ta2] -> getConstantPrimitives ta2 ss
  where
    buildPrim ss = (HV (unsafeCoerce ss), LitE (StringL ss) , AppT (ConT ''[]) (ConT ''Char))
    ss' = map (\s -> reverse (dropWhile (=='\n') $ reverse s))$ nub $ sort ss
