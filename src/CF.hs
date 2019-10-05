{-# LANGUAGE RecordWildCards #-}
module CF where

import Data.Maybe

import Debug.Trace

import CF.CFAPI
import CF.CFConfig
import CF.CFToolWrapper
import           Text.Read                      ( readMaybe )

type ProblemId = (Int, Char)
data Problem = Problem {problemId :: ProblemId, rating :: Int} deriving Show

(=~=) :: String -> String -> Bool
s1 =~= s2 = clean s1 == clean s2
  where
    clean :: String -> String
    clean s = reverse (dropWhile (=='\n') $ reverse s)

{-
getPredicate :: CFConfig -> ProblemId -> IO ((String -> String) -> Bool)
getPredicate cfg problemId = do
  inputOutput <- getInputOutput cfg problemId
  trace (show inputOutput) $ return ()
  return $ \f -> and [f i =~= o | (i, o) <- inputOutput]

extendPredicate
  :: ((String -> String) -> Bool)
  -> (String, String)
  -> ((String -> String) -> Bool)
extendPredicate p (i, o) = trace (show (i, o)) $ \f -> p f && (f i =~= o)
-}

getProblems :: IO [Problem]
getProblems =  map apiProbToProb <$> getAPIProblems
  where
    apiProbToProb :: APIProblem -> Problem
    apiProbToProb APIProblem{..} =
      Problem { problemId = (fromJust contestId, head index)
              , rating = fromJust rating
              }
