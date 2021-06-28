{-# LANGUAGE TemplateHaskell #-}
module CF.CFAPI(getAPIProblems, APIProblem(..)) where

import Data.Aeson         (Options (..))
import Data.Aeson.TH      ()
import Network.Curl.Aeson ()

import qualified Data.Aeson
import qualified Data.Aeson.TH
import qualified Data.List
import qualified Data.Maybe
import qualified Network.Curl.Aeson as Curl.Aeson

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

$(Data.Aeson.TH.deriveJSON Data.Aeson.defaultOptions ''APIProblem)
$(Data.Aeson.TH.deriveJSON Data.Aeson.defaultOptions{fieldLabelModifier = drop 3} ''APIProblemStatistics)
$(Data.Aeson.TH.deriveJSON Data.Aeson.defaultOptions ''APIResponseProblems)
$(Data.Aeson.TH.deriveJSON Data.Aeson.defaultOptions ''APIResponse)

getAPIProblems :: IO [APIProblem]
getAPIProblems  = do
  cfProblems <- problems . result <$>
                Curl.Aeson.curlAesonGet "codeforces.com/api/problemset.problems"
  return $ Data.List.sortOn rating $
      filter (\p -> Data.Maybe.isJust (rating p) && Data.Maybe.isJust (contestId p)) cfProblems
