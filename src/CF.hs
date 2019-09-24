{-# LANGUAGE RecordWildCards #-}
module CF where

import Data.Maybe

import Debug.Trace

import CF.CFAPI
import CF.CFConfig
import CF.CFToolWrapper

type ProblemId = (Int, Char)
data Problem = Problem {problemId :: ProblemId, rating :: Int} deriving Show

getPredicate :: CFConfig -> ProblemId -> IO ((String -> String) -> Bool)
getPredicate cfg problemId = do
  inputOutput <- getInputOutput cfg problemId
  return $ \f -> and [f i =~= o | (i, o) <- inputOutput]
  where
    (=~=) :: String -> String -> Bool
    s1 =~= s2 = clean s1 == clean s2
    clean :: String -> String
    clean s = reverse (dropWhile (=='\n') $ reverse s)

getProblems :: IO [Problem]
getProblems =  map apiProbToProb <$> getAPIProblems
  where
    apiProbToProb :: APIProblem -> Problem
    apiProbToProb APIProblem{..} =
      Problem { problemId = (fromJust contestId, head index)
              , rating = fromJust rating
              }
