{-# LANGUAGE TemplateHaskell #-}

import MagicCP
import CF.CFConfig
import Text.Printf
import MagicHaskeller
import MagicHaskeller.LibTH

import MagicCP.SearchOptions

testsolve wOps wAbs cfg pId = do
  printf "%s %s\n" (show wOps) (show wAbs)
  me <- solveWithAllParsers wOps wAbs cfg customLibrary pId
  case me of
    Just e -> putStrLn $ pprintUC e
    Nothing -> putStrLn "sad"

main = do
  putStrLn "please (ContestId, problemLetter)"
  pId <- read <$> getLine :: IO (Int, Char)
  cfg <- getCFConfig
  testsolve WithOptimizations WithoutAbsents cfg pId
  testsolve WithOptimizations WithAbsents cfg pId
  testsolve WithoutOptimizations WithoutAbsents cfg pId
  --testsolve WithoutOptimizations WithAbsents pId

customLibrary :: [PrimitiveWithOpt]
customLibrary = $(pOpt [|
  (
    ((&&) :: Bool -> Bool -> Bool, [ NotConstantAsFirstArg
                                   , NotConstantAsSecondArg
                                   , CommAndAssoc
                                   , FirstAndSecondArgDifferent ])
  --, ((||) :: Bool -> Bool -> Bool, [ NotConstantAsFirstArg
                                   --, NotConstantAsSecondArg
                                   --, CommAndAssoc
                                   --, FirstAndSecondArgDifferent ])
  --, ((foldl :: (a -> b -> a) -> a -> [b] -> a), [ FirstArgOfFirstArgUsed
                                                --, SecondArgOfFirstArgUsed ])
  --, ((== 1) :: Int -> Bool, [ NotConstantAsFirstArg ])
  --, ((== 0) :: Int -> Bool, [ NotConstantAsFirstArg ])
  --, ((== '7') :: Char -> Bool, [ NotConstantAsFirstArg ])
  --, ((== '4') :: Char -> Bool, [ NotConstantAsFirstArg ])
  --, (list_para :: (->) [b] (a -> (b -> [b] -> a -> a) -> a), [NotConstantAsFirstArg, ThirdArgOfThirdArgUsed])
  --, (show . length . filter (\x -> x == '4' || x == '7') :: [Char] -> [Char], [])
  --, ((>=) :: Int -> Int -> Bool, [ FirstAndSecondArgDifferent ])
  --, (1 :: Int, [])
  --, (3 :: Int, [])
  --, ((1+) :: Int->Int, [])
  --, ((==) :: Char -> Char -> Bool, [ CommAndAssoc
                                   --, FirstAndSecondArgDifferent ])
  --(((== 0) . (`mod` 2)) :: Int -> Bool, [ NotConstantAsFirstArg ])
  --, ((\a b -> a ++ " " ++ b) :: [Char] -> [Char] -> [Char], [ NotConstantAsFirstArg
                                                            --, NotConstantAsSecondArg ])
  , ((flip (-) 1) :: Int->Int, [ NotConstantAsFirstArg ])
  , ((-) :: Int -> Int -> Int, [ NotConstantAsFirstArg, NotConstantAsSecondArg ])
  , ((min) :: Int -> Int -> Int, [ NotConstantAsFirstArg, NotConstantAsSecondArg ])
  , ((abs) :: Int -> Int, [ NotConstantAsFirstArg ])
  --, ((\n -> iF (n `mod` 2 == 0) "I love it" "I hate it") :: Int -> [Char], [NotConstantAsFirstArg])
  ) |] )
