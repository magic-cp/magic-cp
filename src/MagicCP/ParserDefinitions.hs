{-# LANGUAGE TemplateHaskell #-}
module MagicCP.ParserDefinitions where

import Language.Haskell.TH

parseIntListWithSizeDec :: DecsQ
parseIntListWithSizeDec  =
  [d|
  parseIntListWithSize :: String -> Maybe (Int, [Int])
  parseIntListWithSize i = do
    let ls = lines i
    when (length ls /= 2) Nothing
    let [l1, l2] = ls
    n <- readMaybe l1 :: Maybe Int
    as <- mapM readMaybe $ words l2 :: Maybe [Int]
    return (n, as)
    |]

parseIntListIgnoreSizeDec :: DecsQ
parseIntListIgnoreSizeDec  =
  [d|
  parseIntListIgnoreSize :: String -> Maybe [Int]
  parseIntListIgnoreSize i = do
    let ls = lines i
    when (length ls /= 2) Nothing
    let [l1, l2] = ls
    n <- readMaybe l1 :: Maybe Int
    as <- mapM readMaybe $ words l2 :: Maybe [Int]
    when (length as /= n) Nothing
    return as
    |]

parseTwoIntsDec :: DecsQ
parseTwoIntsDec =
  [d|
  parseTwoInts :: String -> Maybe (Int, Int)
  parseTwoInts i = do
    when (length (lines i) /= 1) Nothing
    ns <- mapM readMaybe $ words i :: Maybe [Int]
    when (length ns /= 2) Nothing
    let [a, b] = ns
    return (a, b)
    |]

parseThreeIntsDec :: DecsQ
parseThreeIntsDec  =
  [d|
  parseThreeInts :: String -> Maybe (Int, Int, Int)
  parseThreeInts i = do
    when (length (lines i) /= 1) Nothing
    ns <- mapM readMaybe $ words i :: Maybe [Int]
    when (length ns /= 3) Nothing
    let [a, b, c] = ns
    return (a, b, c)
    |]

parseFourIntsDec :: DecsQ
parseFourIntsDec =
  [d|
  parseFourInts :: String -> Maybe (Int, Int, Int, Int)
  parseFourInts i = do
    when (length (lines i) /= 1) Nothing
    ns <- mapM readMaybe $ words i :: Maybe [Int]
    when (length ns /= 4) Nothing
    let [a, b, c, d] = ns
    return (a, b, c, d)
    |]
