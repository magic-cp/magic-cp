{-# LANGUAGE RecordWildCards #-}
module CF where

import Data.Maybe

import CF.CFAPI

type ProblemId = (Int, Char)
data Problem = Problem {problemId :: ProblemId, rating :: Int} deriving Show

getProblems :: IO [Problem]
getProblems =  map apiProbToProb <$> getAPIProblems
  where
    apiProbToProb :: APIProblem -> Problem
    apiProbToProb APIProblem{..} =
      Problem { problemId = (fromJust contestId, head index)
              , rating = fromJust rating
              }
