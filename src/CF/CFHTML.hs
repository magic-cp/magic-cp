{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module CF.CFHTML( getLastTestCase2 ) where

import CF.CFConfig

import Data.List( isSuffixOf )
import Data.Text( Text, pack, unpack )

import Text.HTML.DOM( parseLBS )
import Text.XML.Cursor( Cursor, fromDocument, child, content, attributeIs
                      , element, (>=>), ($//), (&|), (&//)
                      )

import Network.HTTP.Conduit( simpleHttp )
import           System.Process                 ( readProcess )
import           System.FilePath.Posix          ( (</>) )

-- TODO for TLEs we don't get the jury's answer and this breaks (last of empty).
getLastTestCase :: Int -> Int -> IO (Maybe (String, String))
getLastTestCase cId subId = do
  let cId' = show cId
      subId' = show subId
      url = "https://codeforces.com/contest/" <> cId' <> "/submission/" <> subId'
  cursor <- fromDocument . parseLBS <$> simpleHttp url
  let lastIn = cleanNL . unpack . last . concat $ cursor $// findNodes "file input-view" &| content
  let lastOut = cleanNL . unpack . last . concat $ cursor $// findNodes "file answer-view" &| content
  if "..." `isSuffixOf` lastIn || "..." `isSuffixOf` lastOut
     then return Nothing
     else return $ Just (lastIn, lastOut)
  where
    findNodes :: Text -> Cursor -> [Cursor]
    findNodes divClass = element "div" >=> attributeIs "class" divClass &// element "pre" >=> child
    cleanNL :: String -> String
    cleanNL = filter (/= '\r')

getLastTestCase2 :: CFConfig -> Int -> Int -> IO (Maybe (String, String))
getLastTestCase2 CFConfig{..} cId subId = do
  let cId' = show cId
      subId' = show subId
      url = "https://codeforces.com/contest/" <> cId' <> "/submission/" <> subId'
  io <- readProcess "python" [project_root </> "scripts/sel.py", url] ""
  let (i, o) = span ("##ENDOFINPUT##" /=) $ lines io
      lastIn = unlines i
      lastOut = unlines $ drop 1 o
  if "..." `isSuffixOf` lastIn || "..." `isSuffixOf` lastOut
     then return Nothing
     else return $ Just (lastIn, lastOut)

