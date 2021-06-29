{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module MagicCP where

import qualified Language.Haskell.TH as TH

import CF                       (ProblemId)
import CF.CFConfig              (CFConfig (..))
import CF.CFToolWrapper         (Verdict (..))
import Control.Concurrent       (ThreadId)
import Control.Exception        (SomeException (..), catch)
import Data.Generics            (Data)
import MagicCP.ParseInputOutput (ParseInputOutput, WithTestCases (..))
import MagicCP.SearchOptions    (WithAbsents (..), WithOptimizations (..))
import MagicHaskeller.LibTH
    ( Exp (..)
    , HValue (..)
    , Lit (..)
    , Ppr
    , Primitive
    , PrimitiveWithOpt
    , ProgGen
    , ProgGenSF
    , Type (..)
    , TypeRep
    , Typeable
    )
import System.FilePath.Posix    ((</>))
import Unsafe.Coerce            (unsafeCoerce)

import qualified CF.CFHTML                       as CFHTML
import qualified CF.CFToolWrapper                as CFToolWrapper
import qualified Control.Concurrent
import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Array                      as Array
import qualified Data.Char
import qualified Data.Generics
import qualified Data.List                       as List
import qualified Data.Maybe                      as Maybe
import qualified Data.Time.Clock                 as Clock
import qualified Debug.Trace
import qualified MagicCP.ParseInputOutput        as ParseInputOutput
import qualified MagicCP.Util.ExpressionCnt      as ECnt
import qualified MagicCP.Util.Logger             as Logger
import qualified MagicCP.Util.Memory             as Memory
import qualified MagicCP.Util.Timer              as Timer
import qualified MagicHaskeller.LibTH            as LibTH
import qualified MagicHaskeller.LibTHDefinitions as LibTHDefinitions
import qualified MagicHaskeller.ProgramGenerator as ProgramGenerator
import qualified MagicHaskeller.TimeOut          as TimeOut
import qualified Text.Printf                     as Printf

checkInitialized :: IO ()
checkInitialized = do
  pg <- ProgramGenerator.extractCommon <$> LibTH.getPG
  Control.Monad.when (null $ Array.elems (ProgramGenerator.vl pg) <> Array.elems (ProgramGenerator.pvl pg))
    $ putStrLn "ProgramGenerator not initialized."

generateFile :: ParseInputOutput b => CFConfig -> ProblemId -> b -> WithTestCases -> TH.Exp -> IO ()
generateFile CFConfig{..} (cId, pIndex) hoge wTC e = do
  let parserDecs = ParseInputOutput.parserDeclarations hoge
      mainDecs = ParseInputOutput.mainParserDeclarations hoge wTC
  time <- Clock.getCurrentTime
  program <- LibTH.runQ $ concat <$> sequence
    [ LibTHDefinitions.allDeclarations
    , parserDecs
    , (:[]) <$> LibTH.funD (LibTH.mkName "solve") [LibTH.clause [] (LibTH.normalB $ return e) []]
    , mainDecs
    ]
  writeFile fileName $
    "-- " <> show time <> "\n" <>
    "import Data.Maybe\n" <>
    "import Data.Char\n" <>
    "import Control.Monad\n" <>
    "import Text.Read\n" <>
    pprintUC program
  where
    fileName = cfparse_dir </> show cId </> [Data.Char.toLower pIndex] </> (Data.Char.toLower pIndex:".hs")

pprintUC :: (Ppr a, Data a) => a -> String
pprintUC =  LibTH.pprint . Data.Generics.everywhere (Data.Generics.mkT unqCons)
unqCons :: TH.Name -> TH.Name
unqCons n = LibTH.mkName (LibTH.nameBase n)

data WithOutputConstants = WithOutputConstants | WithoutOutputConstants

solveWithAllParsers
  :: WithOptimizations
  -> WithAbsents
  -> WithOutputConstants
  -> CFConfig
  -> [PrimitiveWithOpt]
  -> ProblemId
  -> IO (Maybe Exp)
solveWithAllParsers wOps wAbs wOC cfg lib pId = do
  let l =
        [ solveWithLimits (solvev0 wOps wAbs wOC WithoutTestCases cfg lib
            (undefined :: Int -> Int -> Int -> String)) pId
        , solveWithLimits (solvev0 wOps wAbs wOC WithTestCases cfg lib
            (undefined :: Int -> Int -> Int -> Int -> Int)) pId
        , solveWithLimits (solvev0 wOps wAbs wOC WithoutTestCases cfg lib
            (undefined :: Int -> Int -> Int)) pId
        , solveWithLimits (solvev0 wOps wAbs wOC WithTestCases cfg lib
            (undefined :: Int -> Int -> String)) pId
        , solveWithLimits (solvev0 wOps wAbs wOC WithoutTestCases cfg lib
            (undefined :: [Int] -> String)) pId
        , solveWithLimits (solvev0 wOps wAbs wOC WithoutTestCases cfg lib
            (undefined :: Int -> [Int] -> String)) pId
        , solveWithLimits (solvev0 wOps wAbs wOC WithoutTestCases cfg lib
            (undefined :: Int -> String)) pId
        , solveWithLimits (solvev0 wOps wAbs wOC WithoutTestCases cfg lib
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
  tid <- Control.Concurrent.myThreadId
  let timeout = 60*30
      memoPerc = 80
  Control.Exception.bracket
    ( Control.Concurrent.forkIO $ checkLimits tid timeout memoPerc )
    Control.Concurrent.killThread
    ( \_ -> Just <$> solve pId )
    --`catch` \(ErrorCall _) -> do
    `catch` \(SomeException _) ->
      return Nothing
  where
  checkLimits :: ThreadId -> Int -> Float -> IO ()
  checkLimits tid tout memLimit = do
    memusage <- Memory.getMemoUsage
    Control.Monad.when (tout `mod` 300 == 0) $ putStrLn $ "checking limits: " ++ show tout ++ "  " ++ show memusage
    if memusage > memLimit || tout < 0
       then do
         putStrLn "Timed out!!"
         Control.Concurrent.killThread tid
       else do
        Control.Concurrent.threadDelay 5000000
        checkLimits tid (tout - 5) memLimit


solvev0
  :: forall b . (Typeable b, ParseInputOutput b)
  => WithOptimizations
  -> WithAbsents
  -> WithOutputConstants
  -> WithTestCases
  -> CFConfig
  -> [PrimitiveWithOpt]
  -> b
  -> ProblemId
  -> IO Exp
solvev0 wOps wAbs wOC wTC cfg customLibrary hoge pId@(cId, _) = do
  putStrLn $ "Parsing problem using " <> ParseInputOutput.parserNameNOTC hoge
  ios <- CFToolWrapper.getInputOutput cfg pId
  print ios
  let initialPred = makePred ios
      custom = case wOC of
              WithOutputConstants ->
                getConstantPrimitives (LibTH.typeOf hoge) (concatMap (words . snd) ios)
              WithoutOutputConstants -> []
      (md :: ProgGen, prims) = if wOps == WithOptimizations
      --(md :: ProgGen, prims) = if wOps == WithOptimizations
              then let (md', lst) = LibTH.mkPGWithDefaultsOpts $
                        customLibrary ++ zip custom (repeat [])
                    in (md', concatMap (\(prim, ops) ->
                          pprintUC prim ++ " " ++ show ops ++ "\n") lst)
              else let (md', lst) = LibTH.mkPGWithDefaults $
                        map fst customLibrary ++ custom
                    in (md', concatMap (\prim -> pprintUC prim ++ "\n") lst)
  initialPred `seq` return ()

  Logger.newLogger (log_root cfg) pId wOps wAbs
  Logger.logParser hoge wTC
  Logger.logPrimitives prims

  Logger.write "ProgGenSF"
  --Logger.write "ProgGen"

  putStrLn "Starting search!"


  Timer.reset
  Timer.start
  ECnt.reset
  --Logger.write "F"
  --let et = everythingF md (wAbs == WithAbsents)
  let et = LibTH.everything md (wAbs == WithAbsents)
      mpto = LibTH.timeout $ ProgramGenerator.opt $ ProgramGenerator.extractCommon md
  f mpto initialPred (concat et)
  where
    makePred :: [(String, String)] -> b -> Bool
    makePred inputOutputs = fromJust' (ParseInputOutput.getPredicate wTC 0 inputOutputs :: Maybe (b -> Bool))

    f :: Maybe Int
      -> (b -> Bool)
      -> [(Exp, b)]
      -> IO Exp
    f mpto predicate ((e, a):ts) = do
      ECnt.cntExp
      es <- ECnt.getTotalExps
      putStrLn $ "Expression #" <> show es
      putStrLn $ "Generated expression " <> pprintUC e
      result <- TimeOut.maybeWithTO2 mpto (predicate a)
      case result of
        Just True -> do
          putStrLn "found solution to predicate"
          Timer.pause
          generateFile cfg pId hoge wTC e
          testVerd <- CFToolWrapper.testSolution cfg pId
          case testVerd of
            Accepted -> do
              secs <- Timer.getTotalSecs
              es <- ECnt.getTotalExps
              putStrLn $ Printf.printf "Submitting to codeforces (%.3fs, %d exps)" secs es
              submitVerd <- CFToolWrapper.submitSolution cfg pId
              Logger.logSubmission (pprintUC e) secs es submitVerd
              case submitVerd of
                Accepted -> do
                  putStrLn $ "Solution accepted in codeforces:\n" <>
                    pprintUC e
                  secs <- Timer.getTotalSecs
                  putStrLn $ Printf.printf "Time: %.3fs" secs
                  es <- ECnt.getTotalExps
                  putStrLn $ Printf.printf "Expressions tried: %d" es
                  return e
                Rejected subm msg -> do
                  putStrLn $ "sumbission #" <> show subm <> " failed with: " <>
                    drop 2 (dropWhile (/= ':') msg)

                  newTestCases <- Maybe.catMaybes <$> CFHTML.getTestCases cfg cId subm
                  putStrLn $ "Got " <> show (length newTestCases) <> " new test cases"
                  print newTestCases
                  Timer.start
                  f mpto (makePred newTestCases) ts
            Rejected{} -> do
              putStrLn "Failed Sample Tests"
              Timer.start
              f mpto predicate ts
        Just False -> do
          putStrLn "Failed"
          f mpto predicate ts
        Nothing -> do
          putStrLn "Timed out"
          f mpto predicate ts
    fromJust' (Just x) = x
    fromJust' _ = error $ "Wrong parser: " ++ ParseInputOutput.parserName hoge wTC


getConstantPrimitives :: TypeRep -> [String] -> [Primitive]
getConstantPrimitives t ss =
  let a@(_, args) = LibTH.splitTyConApp t
      b@(_, _) = LibTH.splitTyConApp (LibTH.typeRep "hoge")
   in if a == b
         then Debug.Trace.trace (show ss') $ map buildPrim ss'
         else case args of
                [] -> []
                [ta1] -> getConstantPrimitives ta1 ss
                [_, ta2] -> getConstantPrimitives ta2 ss
                _ -> error "getConstantPrimitives failed"
  where
    buildPrim ss'' = (HV (unsafeCoerce ss''), LitE (StringL ss'') , AppT (ConT ''[]) (ConT ''Char))
    ss' = map (\s -> reverse (dropWhile (=='\n') $ reverse s)) $ List.nub $ List.sort ss
