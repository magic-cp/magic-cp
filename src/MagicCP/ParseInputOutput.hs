{-# LANGUAGE FlexibleInstances #-}
module MagicCP.ParseInputOutput where

import           Control.Monad                  ( when )
import           Text.Read                      ( readMaybe )
import           Debug.Trace                    ( trace )


type Parser = Int

(&&&) :: (b -> Bool) -> (b -> Bool) -> (b -> Bool)
p1 &&& p2 = \x -> p1 x && p2 x

class ParseInputOutput a where
  getSinglePredicate :: Parser -> (String, String) -> Maybe (a -> Bool)

  getPredicate :: Parser -> [(String, String)] -> Maybe (a -> Bool)
  getPredicate parser l = foldl1 (&&&) <$> mapM (getSinglePredicate parser) l

  extendPredicate :: Parser -> (a -> Bool) -> (String, String) -> Maybe (a -> Bool)
  extendPredicate parser p io = (p &&&) <$> getSinglePredicate parser io

  wut :: a -> String

instance ParseInputOutput (Int -> [Int] -> String) where
  getSinglePredicate 0 (i, o) = do
    trace ("trying to parse: " ++ show (i, o)) $ Just ()
    let ls = lines i
    when (length ls /= 2) Nothing
    let [l1, l2] = ls
    n <- readMaybe l1 :: Maybe Int
    as <- mapM readMaybe $ words l2 :: Maybe [Int]
    trace (show (n, as, o)) $ Just ()

    let los = lines o
    when (length los /= 1) Nothing

    Just (\f -> f n as == head los)
  wut _ = "int -> [int] -> string"

instance ParseInputOutput (String -> String) where
  getSinglePredicate 0 (i, o) = Just (\f -> f i == o)
  wut _ = "string -> string"
