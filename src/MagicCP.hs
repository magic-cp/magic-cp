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

import Unsafe.Coerce(unsafeCoerce)

import CF
import CF.CFConfig
import CF.CFToolWrapper
import CF.CFHTML

import MagicCP.Util.Memory
import MagicCP.ParseInputOutput

import MagicHaskeller hiding ( TH(..) )
import MagicHaskeller.ProgGenSF
import MagicHaskeller.ProgramGenerator
import MagicHaskeller.LibTH( reallyalltest, mkPGWithDefaults )
import MagicHaskeller.LibTHDefinitions
import MagicHaskeller.TimeOut( maybeWithTO2 )


import MagicCP.ParserDefinitions

checkInitialized :: IO ()
checkInitialized = do
  pg <- extractCommon <$> getPG
  when (null $ elems (vl pg) ++ elems (pvl pg)) $ putStrLn "ProgramGenerator not initialized."

generateFile :: CFConfig -> ProblemId -> DecsQ -> TH.Exp -> IO ()
generateFile CFConfig{..} (cId, pIndex) parserDecs e = do
  time <- getCurrentTime
  program <- runQ $ concat <$> sequence
    [ allDeclarations
    , parserDecs
    , (:[]) <$> funD (mkName "solve") [clause [] (normalB $ return e) []]
    , [d| main = getContents >>= \c -> putStrLn (uncurry' solve (parser c)) |]
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

solveWithAllParsers :: ProblemId -> IO (Maybe Exp)
solveWithAllParsers pId = do
  let l = [ solveWithLimits (solvev0 :: ((Int -> Int -> Int -> String) -> ProblemId -> IO Exp)) pId
          , solveWithLimits (solvev0 :: (([Int] -> String) -> ProblemId -> IO Exp)) pId
          , solveWithLimits (solvev0 :: ((Int -> [Int] -> String) -> ProblemId -> IO Exp)) pId
          , solveWithLimits (solvev0 :: ((Int -> String) -> ProblemId -> IO Exp)) pId
          , solveWithLimits (solvev0 :: ((String -> String) -> ProblemId -> IO Exp)) pId
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
  let timeout = 60*60*7
      memoPerc = 85
  bracket
    ( forkIO $ checkLimits tid timeout memoPerc )
    killThread
    ( \_ -> Just <$> solve undefined pId )
    `catch` \(e :: SomeException) -> do
      callCommand "beep -f 800 -l 30 -d 200 -n -f 600 -l 20 -n -f 300 -l 20 -n -f 100 -l 50"
      return Nothing
  where
  checkLimits tid tout memLimit = do
    memusage <- getMemoUsage
    when (tout `mod` 60 == 0) $ putStrLn $ "checking limits: " ++ show tout ++ "  " ++ show memusage
    if memusage > memLimit || tout < 0
       then killThread tid
       else do
        threadDelay 5000000
        checkLimits tid (tout - 5) memLimit

-- solvev0 (undefined :: (Int -> [Int] -> String)) (1030, 'a')
solvev0 :: forall b . (Typeable b, ParseInputOutput b) => b -> ProblemId -> IO Exp
solvev0 hoge pId@(cId, _) = do
  cfg <- getCFConfig

  putStrLn "Parsing problem"
  ios <- getInputOutput cfg pId
  let pred = fromJust (getPredicate 0 ios :: Maybe (b -> Bool))
      custom = getConstantPrimitives (typeOf hoge) (map snd ios)
      md = mkPGWithDefaults $
          $(p [| ((&&) :: Bool -> Bool -> Bool, (>=) :: Int -> Int -> Bool
                 ,(==) :: Int -> Int -> Bool) |] )
          ++ custom
  pred `seq` return ()

  putStrLn "Starting search"
  let et = everything md False
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
      --putStrLn (pprintUC e)
      result <- maybeWithTO2 mpto (pred a)
      case result of
        Just True -> do
          generateFile cfg pId (wut hoge) e
          testVerd <- testSolution cfg pId
          case testVerd of
            Accepted -> do
              submitBeep
              putStrLn "Submitting to codeforces"
              submitVerd <- submitSolution cfg pId
              case submitVerd of
                Accepted -> do
                  acceptedBeep
                  putStrLn $ "Solution accepted in codeforces:\n" <>
                    pprintUC e
                  return e
                Rejected subm msg -> do
                  rejectedBeep
                  putStrLn $ "sumbission #" <> show subm <> " failed with: " <>
                    drop 2 (dropWhile (/= ':') msg)

                  mtc <- getLastTestCase2 cfg cId subm
                  case mtc of
                    Just io -> do
                      putStrLn "Got new test case"
                      f cfg mpto (fromJust $ extendPredicate 0 pred io) ts
                    Nothing -> do
                      putStrLn "Couldn't get new test case"
                      f cfg mpto pred ts
            Rejected{} -> do
              putStrLn "Failed Sample Tests"
              f cfg mpto pred ts
        Just False ->
          f cfg mpto pred ts
        Nothing -> do
          hPutStrLn stderr ("timeout on "++pprintUC e)
          f cfg mpto pred ts
    submitBeep = callCommand "beep -f 800 -l 200 -d 200 -n -f 800 -l 300"
    acceptedBeep = callCommand "beep -f 800 -l 200 -d 200 -n -f 1200 -l 300"
    rejectedBeep = callCommand "beep -f 800 -l 200 -d 200 -n -f 600 -l 300"




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
