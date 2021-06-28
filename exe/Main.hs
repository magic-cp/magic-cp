{-# LANGUAGE TemplateHaskell #-}

import CF                    (ProblemId)
import CF.CFConfig           (CFConfig)
import Control.Concurrent    (threadDelay)
import MagicCP               (WithOutputConstants (..))
import MagicCP.SearchOptions (WithAbsents (..), WithOptimizations (..))
import MagicHaskeller        (AdHocOptimizations (..), PrimitiveWithOpt)
import System.Mem            (performGC)

import qualified CF.CFConfig          as CFConfig
import qualified Data.Char
import qualified MagicCP
import qualified MagicHaskeller       as MH
import qualified MagicHaskeller.LibTH as LibTH
import qualified Text.Printf          as Printf

testsolve
  :: WithOptimizations
  -> WithAbsents
  -> WithOutputConstants
  -> CFConfig
  -> ProblemId
  -> [PrimitiveWithOpt]
  -> IO ()
testsolve wOps wAbs wOC cfg pId lib = do
  Printf.printf "\n##############################\n%s %s\n" (show wOps) (show wAbs)
  me <- MagicCP.solveWithAllParsers wOps wAbs wOC cfg lib pId
  case me of
    Just e -> putStrLn $ MagicCP.pprintUC e
    Nothing -> do
      putStrLn "sad"
      performGC

main :: IO ()
main = do
  putStrLn "please (ContestId, problemLetter)"
  --pId <- read <$> getLine :: IO (Int, Char)
  cfg <- CFConfig.getCFConfig
  --testsolve WithOptimizations WithoutAbsents WithoutOutputConstants cfg (59, 'a') lib59a
  --testsolve WithOptimizations WithAbsents WithoutOutputConstants cfg (59, 'a') lib59a
  --testsolve WithoutOptimizations WithoutAbsents WithoutOutputConstants cfg (59, 'a') lib59a
  --testsolve WithoutOptimizations WithAbsents WithoutOutputConstants cfg (59, 'a') lib59a

  --testsolve WithOptimizations WithoutAbsents WithoutOutputConstants cfg (822, 'a') lib822a
  --testsolve WithOptimizations WithAbsents WithoutOutputConstants cfg (822, 'a') lib822a
  --testsolve WithoutOptimizations WithoutAbsents WithoutOutputConstants cfg (822, 'a') lib822a
  --testsolve WithoutOptimizations WithAbsents WithoutOutputConstants cfg (822, 'a') lib822a

  --testsolve WithOptimizations WithoutAbsents WithOutputConstants cfg (1186, 'a') lib1186a
  --testsolve WithOptimizations WithAbsents WithOutputConstants cfg (1186, 'a') lib1186a
  --testsolve WithoutOptimizations WithoutAbsents WithOutputConstants cfg (1186, 'a') lib1186a
  --testsolve WithoutOptimizations WithAbsents WithOutputConstants cfg (1186, 'a') lib1186a
  --testsolve WithOptimizations WithoutAbsents WithOutputConstants cfg (1186, 'a') lib1186a'
  --testsolve WithOptimizations WithAbsents WithOutputConstants cfg (1186, 'a') lib1186a'
  --testsolve WithoutOptimizations WithoutAbsents WithOutputConstants cfg (1186, 'a') lib1186a'
  --testsolve WithoutOptimizations WithAbsents WithOutputConstants cfg (1186, 'a') lib1186a'

  -- testsolve WithOptimizations WithoutAbsents WithOutputConstants cfg (110, 'a') lib110a'
  -- testsolve WithOptimizations WithAbsents WithOutputConstants cfg (110, 'a') lib110a'
  -- testsolve WithoutOptimizations WithoutAbsents WithOutputConstants cfg (110, 'a') lib110a'
  -- testsolve WithoutOptimizations WithAbsents WithOutputConstants cfg (110, 'a') lib110a'

  testsolve WithOptimizations WithoutAbsents WithOutputConstants cfg (1186, 'a') lib1186a
  testsolve WithOptimizations WithAbsents WithOutputConstants cfg (1186, 'a') lib1186a
  testsolve WithoutOptimizations WithoutAbsents WithOutputConstants cfg (1186, 'a') lib1186a
  testsolve WithoutOptimizations WithAbsents WithOutputConstants cfg (1186, 'a') lib1186a

  --testsolve WithOptimizations WithoutAbsents WithOutputConstants cfg (1257, 'a') lib1257a
  --testsolve WithOptimizations WithAbsents WithOutputConstants cfg (1257, 'a') lib1257a
  --testsolve WithoutOptimizations WithoutAbsents WithOutputConstants cfg (1257, 'a') lib1257a
  --testsolve WithoutOptimizations WithAbsents WithOutputConstants cfg (1257, 'a') lib1257a


  --testsolve WithOptimizations WithoutAbsents WithoutOutputConstants cfg pId lib705a'
  --testsolve WithOptimizations WithAbsents WithoutOutputConstants cfg pId lib705a'
  --testsolve WithoutOptimizations WithoutAbsents WithoutOutputConstants cfg pId lib705a'
  --testsolve WithoutOptimizations WithAbsents WithoutOutputConstants cfg pId lib705a'

andP, orP, iFP :: [PrimitiveWithOpt]
foldP, listParaP :: [PrimitiveWithOpt]
greaterOrEqP, plusOneP, isEvenP, minusOneP, minusP, minP :: [PrimitiveWithOpt]
absP, eqIntP, zeroP :: [PrimitiveWithOpt]
eqCharP :: [PrimitiveWithOpt]
andP = $(MH.pOptSingle [|
  ((&&) :: Bool -> Bool -> Bool, [ NotConstantAsFirstArg
                                 , NotConstantAsSecondArg
                                 , CommAndAssoc
                                 , FirstAndSecondArgDifferent ])
        |])
orP = $(MH.pOptSingle [|
  ((||) :: Bool -> Bool -> Bool, [ NotConstantAsFirstArg
                                 , NotConstantAsSecondArg
                                 , CommAndAssoc
                                 , FirstAndSecondArgDifferent ])
        |])
iFP = $(MH.pOptSingle [|
  (LibTH.iF :: Bool -> a -> a -> a, [ NotConstantAsFirstArg
                              , SecondAndThirdArgDifferent ])
        |])


foldP = $(MH.pOptSingle [|
  ((foldl :: (a -> b -> a) -> a -> [b] -> a), [ FirstArgOfFirstArgUsed
                                              , SecondArgOfFirstArgUsed ])
        |])
listParaP = $(MH.pOptSingle [|
  (LibTH.list_para :: (->) [b] (a -> (b -> [b] -> a -> a) -> a), [NotConstantAsFirstArg
                                                           , ThirdArgOfThirdArgUsed])
        |])
headIntP = $(MH.pOptSingle [|
  (head :: [Int] -> Int, [NotConstantAsFirstArg])
        |])
headCharP = $(MH.pOptSingle [|
  (head :: [Char] -> Char, [NotConstantAsFirstArg])
        |])
tailCharP = $(MH.pOptSingle [|
  (tail :: [Char] -> [Char], [NotConstantAsFirstArg])
        |])
concatCharP = $(MH.pOptSingle [|
  ((:) :: Char -> [Char] -> [Char], [NotConstantAsFirstArg])
        |])
headP = $(MH.pOptSingle [|
  (head :: [a] -> a, [NotConstantAsFirstArg])
        |])
tailP = $(MH.pOptSingle [|
  (tail :: [a] -> [a], [NotConstantAsFirstArg])
        |])
concatP = $(MH.pOptSingle [|
  ((:) :: a -> [a] -> [a], [NotConstantAsFirstArg, NotConstantAsSecondArg])
        |])
emptyListP = $(MH.pOptSingle [|
  ([] :: [a], [])
        |])
joinCharP = $(MH.pOptSingle [|
  ((++) :: [Char] -> [Char] -> [Char], [NotConstantAsFirstArg, NotConstantAsSecondArg])
        |])


greaterOrEqP = $(MH.pOptSingle [|
  ((>=) :: Int -> Int -> Bool, [ FirstAndSecondArgDifferent ])
        |])
plusOneP = $(MH.pOptSingle [|
  ((1+) :: Int->Int, [])
        |])
plusP = $(MH.pOptSingle [|
  ((+) :: (->) Int ((->) Int Int), [CommAndAssoc])
        |])
productP = $(MH.pOptSingle [|
  ((*) :: (->) Int ((->) Int Int), [CommAndAssoc])
        |])
isEvenP = $(MH.pOptSingle [|
  (((== 0) . (`mod` 2)) :: Int -> Bool, [ NotConstantAsFirstArg ])
        |])
minusOneP = $(MH.pOptSingle [|
  ((flip (-) 1) :: Int->Int, [ NotConstantAsFirstArg ])
        |])
minusP = $(MH.pOptSingle [|
  ((-) :: Int -> Int -> Int, [ NotConstantAsFirstArg, NotConstantAsSecondArg ])
        |])
minP = $(MH.pOptSingle [|
  ((min) :: Int -> Int -> Int, [ NotConstantAsFirstArg, NotConstantAsSecondArg ])
        |])
absP = $(MH.pOptSingle [|
  ((abs) :: Int -> Int, [ NotConstantAsFirstArg ])
        |])
eqIntP = $(MH.pOptSingle [|
  ((==) :: Int -> Int -> Bool, [ CommAndAssoc
                               , FirstAndSecondArgDifferent ])
        |])
zeroP = $(MH.pOptSingle [|
  (0 :: Int, [])
        |])
oneP = $(MH.pOptSingle [|
  (1 :: Int, [])
        |])
natParaP = $(MH.pOptSingle [|
  (LibTH.nat_para :: (->) Int (a -> (Int -> a -> a) -> a), [NotConstantAsFirstArg, SecondArgOfThirdArgUsed])
        |])
gcdP = $(MH.pOptSingle [|
  (gcd :: Int -> Int -> Int, [ NotConstantAsFirstArg, NotConstantAsSecondArg ])
        |])


eqCharP = $(MH.pOptSingle [|
  ((==) :: Char -> Char -> Bool, [ CommAndAssoc
                                 , FirstAndSecondArgDifferent ])
        |])
lib1257a :: [PrimitiveWithOpt]
lib1257a = minusOneP ++ minusP ++ absP ++ minP ++ plusP

lib1030a, lib1030a' :: [PrimitiveWithOpt]
lib1030a = foldP ++ headIntP ++ eqIntP ++ zeroP ++ plusOneP ++ iFP
lib1030a' = listParaP ++ headIntP ++ eqIntP ++ zeroP ++ plusOneP ++ iFP

lib1186a, lib1186a' :: [PrimitiveWithOpt]
lib1186a = iFP ++ greaterOrEqP ++ andP
lib1186a' = iFP ++ greaterOrEqP

lib705a, lib705a' :: [PrimitiveWithOpt]
lib705a = isEvenP ++ iFP ++ minusOneP ++ minusP ++ natParaP ++
  $(MH.pOpt [|
  ( ((\n -> LibTH.iF ((n `mod` 2) == 0) "I love it" "I hate it") :: Int -> [Char] , [NotConstantAsFirstArg])
  , (("I hate that " ++) :: [Char] -> [Char], [NotConstantAsFirstArg])
  , (("I love that " ++) :: [Char] -> [Char], [NotConstantAsFirstArg])
  )|])
lib705a' = isEvenP ++ iFP ++ minusOneP ++ minusP ++ natParaP ++
  $(MH.pOpt [| (
    ("I love it" :: [Char], [])
  , ("I hate it" :: [Char], [])
  , (("I hate that " ++) :: [Char] -> [Char], [NotConstantAsFirstArg])
  , (("I love that " ++) :: [Char] -> [Char], [NotConstantAsFirstArg])
  )|])

lib959a :: [PrimitiveWithOpt]
lib959a = isEvenP ++ iFP

lib1257b, lib1257b' :: [PrimitiveWithOpt]
lib1257b = greaterOrEqP ++ iFP ++
  $(MH.pOpt [| (
    (1 :: Int, [])
  , (3 :: Int, [])
  )|])
lib1257b' = greaterOrEqP ++ iFP ++ zeroP ++ plusOneP ++
  $(MH.pOptSingle [|
    (3 :: Int, [])
  |])

lib110a, lib110a' :: [PrimitiveWithOpt]
lib110a = eqCharP ++ foldP ++ iFP ++ orP ++
  $(MH.pOpt [| (
    ((== '4') :: Char -> Bool, [])
  , ((== '7') :: Char -> Bool, [])
  , (show . (length . filter (\x -> (x == '4') || (x == '7'))) :: [Char] -> [Char], [])
  )|])
lib110a' = eqCharP ++ foldP ++ iFP ++ orP ++
  $(MH.pOpt [| (
    ('4' :: Char, [])
  , ('7' :: Char, [])
  , (show . (length . filter (\x -> (x == '4') || (x == '7'))) :: [Char] -> [Char], [])
  --, ((\f x -> show $ (length . filter f) x) :: (Char -> Bool) -> [Char] -> [Char], [])
  )|])

lib281a, lib281a' :: [PrimitiveWithOpt]
lib281a = headCharP ++ tailCharP ++ concatCharP ++
  $(MH.pOptSingle [|
    (Data.Char.toUpper :: Char -> Char, [])
  |])
lib281a' = headCharP ++ tailCharP ++ concatCharP ++ emptyListP ++ joinCharP ++
  $(MH.pOptSingle [|
    (Data.Char.toUpper :: Char -> Char, [])
  |])


lib822a :: [PrimitiveWithOpt]
lib822a = minP ++ natParaP ++ oneP ++ productP ++ plusOneP ++ gcdP

lib59a :: [PrimitiveWithOpt]
lib59a = iFP ++ greaterOrEqP ++ emptyListP ++
  $(MH.pOpt [| (
    ((\f -> length . filter f) :: (Char -> Bool) -> [Char] -> Int, [NotConstantAsSecondArg])
  , (Data.Char.isUpper :: Char -> Bool, [])
  , (Data.Char.isLower :: Char -> Bool, [])
  , (Data.Char.toUpper :: Char -> Char, [])
  , (Data.Char.toLower :: Char -> Char, [])
  , (map :: (a -> b) -> [a] -> [b], [NotConstantAsSecondArg, FirstArgOfFirstArgUsed])
  )|])
