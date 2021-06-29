{-# LANGUAGE RecordWildCards #-}
module CF (ProblemId) where

type ProblemId = (Int, Char)
data Problem = Problem {problemId :: ProblemId, rating :: Int} deriving Show
