{-# LANGUAGE TemplateHaskell #-}
module CF.CFAPI(getAPIProblems, APIProblem(..)) where

import Data.Aeson
import Data.List
import Data.Maybe
import Network.Curl.Aeson
import Data.Aeson.TH

import GHC.Generics
data APIResponse = APIResponse
  { status :: String
  , result :: APIResponseProblems
  } deriving (Show)

data APIResponseProblems = APIResponseProblems
  { problems :: [APIProblem]
  , problemStatistics :: [APIProblemStatistics]
  } deriving (Show)

data APIProblemStatistics = APIProblemStatistics
  { st_contestId :: Maybe Int
  , st_index :: String
  , st_solvedCount :: Int
  } deriving (Show)

data APIProblem = APIProblem
  { contestId :: Maybe Int
  , index :: String
  , rating :: Maybe Int
  , tags :: [String]
  } deriving (Show)

$(deriveJSON defaultOptions ''APIProblem)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''APIProblemStatistics)
$(deriveJSON defaultOptions ''APIResponseProblems)
$(deriveJSON defaultOptions ''APIResponse)

getAPIProblems :: IO [APIProblem]
getAPIProblems  = do
  problems <- problems . result <$>
                curlAesonGet "codeforces.com/api/problemset.problems"
  return $ sortOn rating $
      filter (\p -> isJust (rating p) && isJust (contestId p)) problems
