{-# LANGUAGE TemplateHaskell #-}
module MagicCP.ParserDefinitions where

import Language.Haskell.TH

parse1InputDec :: DecsQ
parse1InputDec  =
  [d|
  parse1Input :: String -> Maybe (Int, [Int])
  parse1Input i = do
    let ls = lines i
    when (length ls /= 2) Nothing
    let [l1, l2] = ls
    n <- readMaybe l1 :: Maybe Int
    as <- mapM readMaybe $ words l2 :: Maybe [Int]
    return (n, as)
    |]

parse2InputDec :: DecsQ
parse2InputDec  =
  [d|
  parse2Input :: String -> Maybe (Int, Int, Int)
  parse2Input i = do
    when (length (lines i) /= 1) Nothing
    ns <- mapM readMaybe $ words i :: Maybe [Int]
    when (length ns /= 3) Nothing
    let [a, b, c] = ns
    return (a, b, c)
    |]


parse3InputDec :: DecsQ
parse3InputDec  =
  [d|
  parse3Input :: String -> Maybe [Int]
  parse3Input i = do
    let ls = lines i
    when (length ls /= 2) Nothing
    let [l1, l2] = ls
    n <- readMaybe l1 :: Maybe Int
    as <- mapM readMaybe $ words l2 :: Maybe [Int]
    when (length as /= n) Nothing
    return as
    |]
