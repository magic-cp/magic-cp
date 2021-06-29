{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module MagicCP.ParserDefinitions where

import Language.Haskell.TH

import qualified Control.Monad
import qualified Text.Read

parseIntListWithSizeDec :: DecsQ
parseIntListWithSizeDec  =
  [d|
  parseIntListWithSize :: String -> Maybe (Int, [Int])
  parseIntListWithSize i = do
    let ls = lines i
    Control.Monad.when (length ls /= 2) Nothing
    let [l1, l2] = ls
    n <- Text.Read.readMaybe l1 :: Maybe Int
    as <- mapM Text.Read.readMaybe $ words l2 :: Maybe [Int]
    return (n, as)
    |]

parseIntListIgnoreSizeDec :: DecsQ
parseIntListIgnoreSizeDec  =
  [d|
  parseIntListIgnoreSize :: String -> Maybe [Int]
  parseIntListIgnoreSize i = do
    let ls = lines i
    Control.Monad.when (length ls /= 2) Nothing
    let [l1, l2] = ls
    n <- Text.Read.readMaybe l1 :: Maybe Int
    as <- mapM Text.Read.readMaybe $ words l2 :: Maybe [Int]
    Control.Monad.when (length as /= n) Nothing
    return as
    |]

parseTwoIntsDec :: DecsQ
parseTwoIntsDec =
  [d|
  parseTwoInts :: String -> Maybe (Int, Int)
  parseTwoInts i = do
    Control.Monad.when (length (lines i) /= 1) Nothing
    ns <- mapM Text.Read.readMaybe $ words i :: Maybe [Int]
    Control.Monad.when (length ns /= 2) Nothing
    let [a, b] = ns
    return (a, b)
    |]

parseThreeIntsDec :: DecsQ
parseThreeIntsDec  =
  [d|
  parseThreeInts :: String -> Maybe (Int, Int, Int)
  parseThreeInts i = do
    Control.Monad.when (length (lines i) /= 1) Nothing
    ns <- mapM Text.Read.readMaybe $ words i :: Maybe [Int]
    Control.Monad.when (length ns /= 3) Nothing
    let [a, b, c] = ns
    return (a, b, c)
    |]

parseFourIntsDec :: DecsQ
parseFourIntsDec =
  [d|
  parseFourInts :: String -> Maybe (Int, Int, Int, Int)
  parseFourInts i = do
    Control.Monad.when (length (lines i) /= 1) Nothing
    ns <- mapM Text.Read.readMaybe $ words i :: Maybe [Int]
    Control.Monad.when (length ns /= 4) Nothing
    let [a, b, c, d] = ns
    return (a, b, c, d)
    |]

parseStringsWithoutSizeDec :: DecsQ
parseStringsWithoutSizeDec =
  [d|
  parseStringsWithoutSize :: String -> Maybe [String]
  parseStringsWithoutSize (lines -> _ : s) = Just s
  parseStringsWithoutSize _ = Nothing
    |]
