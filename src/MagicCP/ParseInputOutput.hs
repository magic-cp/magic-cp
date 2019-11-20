{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

data WithTestCases = WithTestCases | WithoutTestCases deriving Show

class Typeable a => ParseInputOutput a where
  getSinglePredicate :: WithTestCases -> Parser -> (String, String) -> Maybe (a -> Bool)
  getSinglePredicate WithoutTestCases p (i, o) = getSinglePredicateNOTC p (i, o)
  getSinglePredicate WithTestCases p (i, o) = do
    let is = tail $ lines i
        os = lines o
    when (length is == 0) Nothing
    when (length is /= length os) Nothing
    foldl1 (&&&) <$> mapM (getSinglePredicate WithoutTestCases p) (zip is os)

  getSinglePredicateNOTC :: Parser -> (String, String) -> Maybe (a -> Bool)

  getPredicate :: WithTestCases -> Parser -> [(String, String)] -> Maybe (a -> Bool)
  getPredicate wTC parser l = foldl1 (&&&) <$> mapM (getSinglePredicate wTC parser) l

  extendPredicate :: WithTestCases -> Parser -> (a -> Bool) -> (String, String) -> Maybe (a -> Bool)
  extendPredicate wTC parser p io = (p &&&) <$> getSinglePredicate wTC parser io

  parserDeclarations :: a -> DecsQ

  mainParserDeclarations :: a -> WithTestCases -> DecsQ
  mainParserDeclarations _ WithoutTestCases =
    [d|
    main = getContents >>= \c -> putStrLn (uncurry' solve (parser c))
    |]
  mainParserDeclarations _ WithTestCases =
    [d|
    main = getContents >>= \c -> mapM_ (putStrLn . uncurry' solve . parser) . tail . lines $ c
    |]

  parserName :: a -> WithTestCases -> String
  parserName a WithTestCases = "Test cases of " ++ parserName a WithoutTestCases
  parserName a WithoutTestCases = parserNameNOTC a

  parserNameNOTC :: a -> String

$(parseTwoIntsDec)
$(parseThreeIntsDec)
$(parseFourIntsDec)

$(parseIntListWithSizeDec)
$(parseIntListIgnoreSizeDec)

instance ParseInputOutput (Int -> [Int] -> String) where
  getSinglePredicateNOTC 0 (i, o) = do
    (n, as) <- parseIntListWithSize i
    let los = lines o
    when (length los /= 1) Nothing
    return (\f -> f n as == head los)
  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' = uncurry
    parser :: String -> (Int, [Int])
    parser = fromJust . parseIntListWithSize
      |],
    parseIntListWithSizeDec
                                             ]
  parserNameNOTC _ = "[Int] with size to String"

instance ParseInputOutput ([Int] -> String) where
  getSinglePredicateNOTC 0 (i, o) = do
    as <- parseIntListIgnoreSize i
    let los = lines o
    when (length los /= 1) Nothing
    return (\f -> f as == head los)
  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' = id
    parser :: String -> [Int]
    parser = fromJust . parseIntListIgnoreSize
      |],
    parseIntListIgnoreSizeDec
                                             ]
  parserNameNOTC _ = "[Int] (ignoring size) to String"

instance ParseInputOutput (String -> String) where
  getSinglePredicateNOTC 0 (i, o) = do
    let los = lines o
    when (length los /= 1) Nothing
    Just (\f -> f i == head los)
  parserDeclarations _ =
    [d|
    uncurry' = id
    parser :: String -> String
    parser = id
      |]
  parserNameNOTC _ = "String to String"

instance ParseInputOutput (Int -> String) where
  getSinglePredicateNOTC 0 (i, o) = do
    ni <- readMaybe i
    let los = lines o
    when (length los /= 1) Nothing
    return (\f -> f ni == head los)
  parserDeclarations _ =
    [d|
    uncurry' = id
    parser :: String -> Int
    parser = read
      |]
  parserNameNOTC _ = "Int to String"

instance ParseInputOutput (Int -> Int -> String) where
  getSinglePredicateNOTC 0 (i, o) = do
    (a, b) <- parseTwoInts i
    let los = lines o
    when (length los /= 1) Nothing
    return (\f -> f a b == head los)
  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' f (a,b) = f a b
    parser :: String -> (Int, Int)
    parser = fromJust . parseTwoInts
      |],
    parseTwoIntsDec
                                             ]
  parserNameNOTC _ = "Two Ints to String"

instance ParseInputOutput (Int -> Int -> Int -> String) where
  getSinglePredicateNOTC 0 (i, o) = do
    (a, b, c) <- parseThreeInts i
    let los = lines o
    when (length los /= 1) Nothing
    return (\f -> f a b c == head los)
  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' f (a,b,c) = f a b c
    parser :: String -> (Int, Int, Int)
    parser = fromJust . parseThreeInts
      |],
    parseThreeIntsDec
                                             ]
  parserNameNOTC _ = "Three Ints to String"

instance ParseInputOutput (Int -> Int -> Int) where
  getSinglePredicateNOTC 0 (i, o) = do
    (a, b) <- parseTwoInts i
    let los = lines o
    when (length los /= 1) Nothing
    return (\f -> f a b == read (head los))
  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' f (a,b) = show $ f a b
    parser :: String -> (Int, Int)
    parser = fromJust . parseTwoInts
      |],
    parseTwoIntsDec
                                             ]
  parserNameNOTC _ = "Two Ints to Int"

instance ParseInputOutput (Int -> Int -> Int -> Int -> Int) where
  getSinglePredicateNOTC 0 (i, o) = do
    (a, b, c, d) <- parseFourInts i
    let los = lines o
    when (length los /= 1) Nothing
    return (\f -> f a b c d == read (head los))
  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' f (a,b,c,d) = show $ f a b c d
    parser :: String -> (Int, Int, Int, Int)
    parser = fromJust . parseFourInts
      |],
    parseFourIntsDec
                                             ]
  parserNameNOTC _ = "Four Ints to Int"
