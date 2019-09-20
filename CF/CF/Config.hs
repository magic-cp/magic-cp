{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module CF.Config
  ( CPConfig(..)
  , getCPConfig
  )where

import Control.Exception

import Data.Configurator
import Data.Configurator.Types

data CPConfig = CPConfig
  { config_cftool_path :: FilePath
  , config_project_root :: FilePath
  , config_cfparse_dir :: FilePath
  } deriving Show

getCPConfig :: IO CPConfig
getCPConfig = do
  cfg <- try $ do
    cfg <- load [Required "config.cfg"]
    cftool_path <- require cfg "cf-tool-path"
    project_root <- require cfg "project-root"
    cf_parse_dir <- require cfg "cf-parse-dir"
    return $ CPConfig { config_cftool_path = cftool_path
                      , config_project_root = project_root
                      , config_cfparse_dir = cf_parse_dir
                      }
  case cfg of
    Right cfg' -> return cfg'
    Left (e :: SomeException) -> do
      putStrLn $ "Exception thrown while loading config.cfg file.\n"
              <> "Read README for more information."
      throw e
