{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module MagicCP.ParseInputOutput where

import           Control.Monad                  ( when )
import           Text.Read                      ( readMaybe )
import           Debug.Trace                    ( trace )
import           Data.Typeable

import Language.Haskell.TH

import MagicCP.ParserDefinitions

type Parser = Int

(&&&) :: (b -> Bool) -> (b -> Bool) -> (b -> Bool)
p1 &&& p2 = \x -> p1 x && p2 x

class Typeable a => ParseInputOutput a where
  getSinglePredicate :: Parser -> (String, String) -> Maybe (a -> Bool)

  getPredicate :: Parser -> [(String, String)] -> Maybe (a -> Bool)
  getPredicate parser l = foldl1 (&&&) <$> mapM (getSinglePredicate parser) l

  extendPredicate :: Parser -> (a -> Bool) -> (String, String) -> Maybe (a -> Bool)
  extendPredicate parser p io = (p &&&) <$> getSinglePredicate parser io

  wut :: a -> DecsQ

$(parse1InputDec)
instance ParseInputOutput (Int -> [Int] -> String) where
  getSinglePredicate 0 (i, o) = do
    (n, as) <- parse1Input i
    let los = lines o
    when (length los /= 1) Nothing
    return (\f -> f n as == head los)
  wut _ = concat <$> sequence [
    [d|
    uncurry' = uncurry
    parser :: String -> (Int, [Int])
    parser = fromJust . parse1Input
      |],
    parse1InputDec
                               ]

$(parse3InputDec)
instance ParseInputOutput ([Int] -> String) where
  getSinglePredicate 0 (i, o) = do
    as <- parse3Input i
    let los = lines o
    when (length los /= 1) Nothing
    return (\f -> f as == head los)
  wut _ = concat <$> sequence [
    [d|
    uncurry' = id
    parser :: String -> [Int]
    parser = fromJust . parse3Input
      |],
    parse3InputDec
                               ]

instance ParseInputOutput (String -> String) where
  getSinglePredicate 0 (i, o) = Just (\f -> f i == o)
  wut _ =
    [d|
    uncurry' = id
    parser :: String -> String
    parser = id
      |]

instance ParseInputOutput (Int -> String) where
  getSinglePredicate 0 (i, o) = do
    ni <- readMaybe i
    return (\f -> f ni == o)
  wut _ =
    [d|
    uncurry' = id
    parser :: String -> Int
    parser = read
      |]


$(parse2InputDec)
instance ParseInputOutput (Int -> Int -> Int -> String) where
  getSinglePredicate 0 (i, o) = do
    (a, b, c) <- parse2Input i
    let los = lines o
    when (length los /= 1) Nothing
    return (\f -> f a b c == head los)
  wut _ = concat <$> sequence [
    [d|
    uncurry' f (a,b,c) = f a b c
    parser :: String -> (Int, Int, Int)
    parser = fromJust . parse2Input
      |],
    parse2InputDec
                               ]
