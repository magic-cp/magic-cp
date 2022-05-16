{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ViewPatterns        #-}

module MagicCP.ParseInputOutput where

import Language.Haskell.TH (DecsQ)
import Data.Map (Map)

import qualified Control.Monad
import qualified MagicCP.ParserDefinitions as ParserDefinitions
import qualified Text.Read
import qualified Data.Maybe
import qualified Data.Map as Map

(&&&) :: Predicate b -> Predicate b -> Predicate b
Predicate {predicateFun = p1, ..} &&& Predicate {predicateFun = p2} =
  Predicate {predicateFun = \x -> p1 x && p2 x, ..}

data WithTestCases = WithTestCases | WithoutTestCases deriving Show

{-| A "Predicate a" holds a predicate applier, to test a solution
    against our inputs/outputs pairs, and an id to identify the way
    the parser used to get this predicate
-}
data Predicate a = Predicate
  { predicateFun :: a -> Bool
  , predicateId :: String
  }

class ParseInputOutput a where
  getSinglePredicateNOTC :: (String, String) -> Map String (Predicate a)
  parserDeclarations :: a -> DecsQ

  getSinglePredicate :: WithTestCases -> (String, String) -> Map String (Predicate a)
  getSinglePredicate WithoutTestCases (i, o) = getSinglePredicateNOTC (i, o)
  getSinglePredicate WithTestCases (i, o) =
    let is = tail $ lines i
        os = lines o
        inputEmpty = null is
        outputInputLengthMismatch = null is
    in  if inputEmpty || outputInputLengthMismatch
        then Map.empty
        else
          Map.unionsWith (&&&) $ zipWith (curry (getSinglePredicate WithoutTestCases)) is os


  getPredicate :: WithTestCases -> [(String, String)] -> Map String (Predicate a)
  getPredicate wTC l = Map.unionsWith (&&&) $ map (getSinglePredicate wTC) l

  extendPredicate :: WithTestCases -> Predicate a -> (String, String) -> Map String (Predicate a)
  extendPredicate wTC p io = (p &&&) <$> getSinglePredicate wTC io


  mainParserDeclarations :: a -> WithTestCases -> DecsQ
  mainParserDeclarations _ WithoutTestCases =
    [d|
    main = getContents >>= \c -> putStrLn (uncurry' solve (parser c))
    |]
  mainParserDeclarations _ WithTestCases =
    [d|
    main = getContents >>= \c -> mapM_ (putStrLn . uncurry' solve . parser) . tail . lines $ c
    |]

  -- parserName :: a -> WithTestCases -> String
  -- parserName a WithTestCases = "Test cases of " ++ parserName a WithoutTestCases
  -- parserName a WithoutTestCases = parserNameNOTC a

  parserNameNOTC :: a -> String

$(ParserDefinitions.parseTwoIntsDec)
$(ParserDefinitions.parseThreeIntsDec)
$(ParserDefinitions.parseFourIntsDec)

$(ParserDefinitions.parseIntListWithSizeDec)
$(ParserDefinitions.parseIntListIgnoreSizeDec)

$(ParserDefinitions.parseStringsWithoutSizeDec)

instance ParseInputOutput (Int -> [Int] -> String) where
  getSinglePredicateNOTC (i, o) = Map.fromList $ Data.Maybe.catMaybes [p]
    where
      p = do
        (n, as) <- parseIntListWithSize i
        let los = lines o
        Control.Monad.when (length los /= 1) Nothing
        let predicateFun = (\f -> f n as == head los)
        let predicateId = "first-line-int-n-ints-follow"
        return (predicateId, Predicate{..})

  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' = uncurry
    parser :: String -> (Int, [Int])
    parser = fromJust . parseIntListWithSize
      |],
    ParserDefinitions.parseIntListWithSizeDec
                                             ]
  parserNameNOTC _ = "[Int] with size to String"

instance ParseInputOutput ([Int] -> String) where
  getSinglePredicateNOTC (i, o) = Map.fromList $ Data.Maybe.catMaybes [p]
    where
      p = do
        as <- parseIntListIgnoreSize i
        let los = lines o
        Control.Monad.when (length los /= 1) Nothing
        let predicateFun = (\f -> f as == head los)
        let predicateId = "n-ints-follow"
        return (predicateId, Predicate{..})

  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' = id
    parser :: String -> [Int]
    parser = fromJust . parseIntListIgnoreSize
      |],
    ParserDefinitions.parseIntListIgnoreSizeDec
                                             ]
  parserNameNOTC _ = "[Int] (ignoring size) to String"

instance ParseInputOutput (String -> String) where
  getSinglePredicateNOTC (i, o) = Map.fromList $ Data.Maybe.catMaybes [p]
    where
      p = do
        let los = lines o
        Control.Monad.when (length los /= 1) Nothing
        let predicateFun = (\f -> f (head $ lines i) == head los)
        let predicateId = "whole-input"
        return (predicateId, Predicate{..})

  parserDeclarations _ =
    [d|
    uncurry' = id
    parser :: String -> String
    parser = head . lines
      |]
  parserNameNOTC _ = "String to String"

instance ParseInputOutput (Int -> String) where
  getSinglePredicateNOTC (i, o) = Map.fromList $ Data.Maybe.catMaybes [p]
    where
      p = do
        ni <- Text.Read.readMaybe i
        let los = lines o
        Control.Monad.when (length los /= 1) Nothing
        let predicateFun = (\f -> f ni == head los)
        let predicateId = "single-int-string"
        return (predicateId, Predicate{..})

  parserDeclarations _ =
    [d|
    uncurry' = id
    parser :: String -> Int
    parser = read
      |]
  parserNameNOTC _ = "Int to String"

instance ParseInputOutput (Int -> Int -> String) where
  getSinglePredicateNOTC (i, o) = Map.fromList $ Data.Maybe.catMaybes [p]
    where
      p = do
        (a, b) <- parseTwoInts i
        let los = lines o
        Control.Monad.when (length los /= 1) Nothing
        let predicateFun = (\f -> f a b == head los)
        let predicateId = "two-ints-string"
        return (predicateId, Predicate{..})

  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' f (a,b) = f a b
    parser :: String -> (Int, Int)
    parser = fromJust . parseTwoInts
      |],
    ParserDefinitions.parseTwoIntsDec
                                             ]
  parserNameNOTC _ = "Two Ints to String"

instance ParseInputOutput (Int -> Int -> Int -> String) where
  getSinglePredicateNOTC (i, o) = Map.fromList $ Data.Maybe.catMaybes [p]
    where
      p = do
        (a, b, c) <- parseThreeInts i
        let los = lines o
        Control.Monad.when (length los /= 1) Nothing
        let predicateFun = (\f -> f a b c == head los)
        let predicateId = "three-ints-string"
        return (predicateId, Predicate{..})

  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' f (a,b,c) = f a b c
    parser :: String -> (Int, Int, Int)
    parser = fromJust . parseThreeInts
      |],
    ParserDefinitions.parseThreeIntsDec
                                             ]
  parserNameNOTC _ = "Three Ints to String"

instance ParseInputOutput (Int -> Int -> Int) where
  getSinglePredicateNOTC (i, o) = Map.fromList $ Data.Maybe.catMaybes [p]
    where
      p = do
        (a, b) <- parseTwoInts i
        let los = lines o
        Control.Monad.when (length los /= 1) Nothing
        let predicateFun = (\f -> f a b == read (head los))
        let predicateId = "two-ints-to-int"
        return (predicateId, Predicate{..})

  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' f (a,b) = show $ f a b
    parser :: String -> (Int, Int)
    parser = fromJust . parseTwoInts
      |],
    ParserDefinitions.parseTwoIntsDec
                                             ]
  parserNameNOTC _ = "Two Ints to Int"

instance ParseInputOutput (Int -> Int -> Int -> Int -> Int) where
  getSinglePredicateNOTC (i, o) = Map.fromList $ Data.Maybe.catMaybes [p]
    where
      p = do
        (a, b, c, d) <- parseFourInts i
        let los = lines o
        Control.Monad.when (length los /= 1) Nothing
        let predicateFun = (\f -> f a b c d == read (head los))
        let predicateId = "four-ints-to-int"
        return (predicateId, Predicate{..})


  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' f (a,b,c,d) = show $ f a b c d
    parser :: String -> (Int, Int, Int, Int)
    parser = fromJust . parseFourInts
      |],
    ParserDefinitions.parseFourIntsDec
                                             ]

  parserNameNOTC _ = "Four Ints to Int"


instance ParseInputOutput ([String] -> String) where
  getSinglePredicateNOTC (i, o) = Map.fromList $ Data.Maybe.catMaybes [p]
    where
      p = do
        let los = lines o
        tailLines <- parseStringsWithoutSize i
        Control.Monad.when (length los /= 1) Nothing
        let predicateFun = (\f -> f tailLines == head los)
        let predicateId = "n-strings-follow"
        return (predicateId, Predicate{..})

  parserDeclarations _ = concat <$> sequence [ 
    [d|
      uncurry' = id
      
      parser :: String -> [String]
      parser = fromJust . parseStringsWithoutSize
      |],
    ParserDefinitions.parseStringsWithoutSizeDec ]

  parserNameNOTC _ = "List of String to String"

