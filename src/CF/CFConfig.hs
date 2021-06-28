{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CF.CFConfig
  ( CFConfig(..)
  , getCFConfig
  )where

import Control.Exception       (SomeException)
import Data.Configurator       (Worth (..))
import Data.Configurator.Types ()

import qualified Control.Exception as Exception
import qualified Data.Configurator as Configurator

data CFConfig = CFConfig
  { cftool_path :: FilePath
  , project_root :: FilePath
  , cfparse_dir :: FilePath
  , log_root :: FilePath
  } deriving Show

getCFConfig :: IO CFConfig
getCFConfig = do
  cfg <- Exception.try $ do
    cfg <- Configurator.load [Required "config.cfg"]
    cftool_path <- Configurator.require cfg "cf-tool-path"
    project_root <- Configurator.require cfg "project-root"
    cfparse_dir <- Configurator.require cfg "cf-parse-dir"
    log_root <- Configurator.require cfg "log-root"
    return $ CFConfig {..}
  case cfg of
    Right cfg' -> return cfg'
    Left (e :: SomeException) -> do
      putStrLn $ "Exception thrown while loading config.cfg file.\n"
              <> "Read README for more information."
      Exception.throw e
