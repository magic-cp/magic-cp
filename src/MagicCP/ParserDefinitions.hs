{-# LANGUAGE TemplateHaskell #-}
module MagicCP.ParserDefinitions where

import Language.Haskell.TH

allParserDefinitions :: DecsQ
allParserDefinitions = concat <$> sequence [parser1Def]

parser1Def :: DecsQ
parser1Def =
  [d|
  parser1 :: String -> (Int, [Int])
  parser1 i = (n, as)
    where [l1, l2] = lines i
          n = read l1
          as = map read $ words l2
    |]
