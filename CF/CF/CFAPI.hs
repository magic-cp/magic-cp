{-# LANGUAGE DeriveGeneric #-}
module CF.CFAPI(getAPIProblems, APIProblem(..)) where

import Data.Aeson
import Data.List
import Data.Maybe
import Network.Curl.Aeson

import GHC.Generics
data APIResponse = APIResponse
  { status :: String
  , result :: APIResponseProblems
  } deriving (Generic, Show)

newtype APIResponseProblems =
  APIResponseProblems { problems :: [APIProblem] } deriving (Generic, Show)

data APIProblem = APIProblem
  { contestId :: Maybe Int
  , index :: String
  , rating :: Maybe Int
  , tags :: [String]
  } deriving (Generic, Show)

instance FromJSON APIResponse
instance FromJSON APIResponseProblems
instance FromJSON APIProblem


getAPIProblems :: IO [APIProblem]
getAPIProblems  = do
  problems <- problems . result <$>
                curlAesonGet "codeforces.com/api/problemset.problems"
  return $ sortOn rating $
      filter (\p -> isJust (rating p) && isJust (contestId p)) problems
