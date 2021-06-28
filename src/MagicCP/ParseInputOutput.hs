{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module MagicCP.ParseInputOutput where

import Language.Haskell.TH (DecsQ)

import qualified Control.Monad
import qualified MagicCP.ParserDefinitions as ParserDefinitions
import qualified Text.Read

type Parser = Int

(&&&) :: (b -> Bool) -> (b -> Bool) -> (b -> Bool)
p1 &&& p2 = \x -> p1 x && p2 x

data WithTestCases = WithTestCases | WithoutTestCases deriving Show

class ParseInputOutput a where
  -- TODO: The type signature for this should be:
  -- getSinglePredicate :: WithTestCases -> Parser -> ([String], String)
  getSinglePredicate :: WithTestCases -> Parser -> (String, String) -> Maybe (a -> Bool)
  getSinglePredicate WithoutTestCases p (i, o) = getSinglePredicateNOTC p (i, o)
  getSinglePredicate WithTestCases p (i, o) = do
    let is = tail $ lines i
        os = lines o
    Control.Monad.when (null is) Nothing
    Control.Monad.when (length is /= length os) Nothing
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

$(ParserDefinitions.parseTwoIntsDec)
$(ParserDefinitions.parseThreeIntsDec)
$(ParserDefinitions.parseFourIntsDec)

$(ParserDefinitions.parseIntListWithSizeDec)
$(ParserDefinitions.parseIntListIgnoreSizeDec)

instance ParseInputOutput (Int -> [Int] -> String) where
  getSinglePredicateNOTC 0 (i, o) = do
    (n, as) <- parseIntListWithSize i
    let los = lines o
    Control.Monad.when (length los /= 1) Nothing
    return (\f -> f n as == head los)
  getSinglePredicateNOTC i _ = error $ mkParserNotImplMsg i "Int -> [Int] -> String"

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
  getSinglePredicateNOTC 0 (i, o) = do
    as <- parseIntListIgnoreSize i
    let los = lines o
    Control.Monad.when (length los /= 1) Nothing
    return (\f -> f as == head los)
  getSinglePredicateNOTC i _ = error $ mkParserNotImplMsg i "[Int] -> String"
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
  getSinglePredicateNOTC 0 (i, o) = do
    let los = lines o
    Control.Monad.when (length los /= 1) Nothing
    Just (\f -> f (head $ lines i) == head los)
  getSinglePredicateNOTC i _ = error $ mkParserNotImplMsg i "String -> String"

  parserDeclarations _ =
    [d|
    uncurry' = id
    parser :: String -> String
    parser = head . lines
      |]
  parserNameNOTC _ = "String to String"

instance ParseInputOutput (Int -> String) where
  getSinglePredicateNOTC 0 (i, o) = do
    ni <- Text.Read.readMaybe i
    let los = lines o
    Control.Monad.when (length los /= 1) Nothing
    return (\f -> f ni == head los)
  getSinglePredicateNOTC i _ = error $ mkParserNotImplMsg i "Int -> String"

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
    Control.Monad.when (length los /= 1) Nothing
    return (\f -> f a b == head los)
  getSinglePredicateNOTC i _ = error $ mkParserNotImplMsg i "Int -> Int -> String"

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
  getSinglePredicateNOTC 0 (i, o) = do
    (a, b, c) <- parseThreeInts i
    let los = lines o
    Control.Monad.when (length los /= 1) Nothing
    return (\f -> f a b c == head los)

  getSinglePredicateNOTC i _ =
    error $ mkParserNotImplMsg i "Int -> Int -> Int -> String"

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
  getSinglePredicateNOTC 0 (i, o) = do
    (a, b) <- parseTwoInts i
    let los = lines o
    Control.Monad.when (length los /= 1) Nothing
    return (\f -> f a b == read (head los))
  getSinglePredicateNOTC i _ = error $ mkParserNotImplMsg i "Int -> Int -> Int"

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
  getSinglePredicateNOTC 0 (i, o) = do
    (a, b, c, d) <- parseFourInts i
    let los = lines o
    Control.Monad.when (length los /= 1) Nothing
    return (\f -> f a b c d == read (head los))

  getSinglePredicateNOTC i _ =
    error $ mkParserNotImplMsg i "Int -> Int -> Int -> Int -> Int"

  parserDeclarations _ = concat <$> sequence [
    [d|
    uncurry' f (a,b,c,d) = show $ f a b c d
    parser :: String -> (Int, Int, Int, Int)
    parser = fromJust . parseFourInts
      |],
    ParserDefinitions.parseFourIntsDec
                                             ]
  parserNameNOTC _ = "Four Ints to Int"


mkParserNotImplMsg :: Parser -> String -> String
mkParserNotImplMsg p s =
  "Parser #" <> show p <> " for " <> s <> " is not implemented"
