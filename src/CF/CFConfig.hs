{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module CF.CFConfig
  ( CFConfig(..)
  , getCFConfig
  )where

import Control.Exception

import Data.Configurator
import Data.Configurator.Types

data CFConfig = CFConfig
  { cftool_path :: FilePath
  , project_root :: FilePath
  , cfparse_dir :: FilePath
  , log_root :: FilePath
  } deriving Show

getCFConfig :: IO CFConfig
getCFConfig = do
  cfg <- try $ do
    cfg <- load [Required "config.cfg"]
    cftool_path <- require cfg "cf-tool-path"
    project_root <- require cfg "project-root"
    cf_parse_dir <- require cfg "cf-parse-dir"
    log_root <- require cfg "log-root"
    return $ CFConfig { cftool_path = cftool_path
                      , project_root = project_root
                      , cfparse_dir = cf_parse_dir
                      , log_root = log_root
                      }
  case cfg of
    Right cfg' -> return cfg'
    Left (e :: SomeException) -> do
      putStrLn $ "Exception thrown while loading config.cfg file.\n"
              <> "Read README for more information."
      throw e
