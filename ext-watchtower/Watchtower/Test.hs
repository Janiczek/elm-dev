module Watchtower.Test where

import Control.Concurrent (threadDelay)
import qualified System.Directory as Dir
import qualified System.Environment as Env

import Ext.Common
import qualified Ext.Log
import qualified Watchtower.Server
import qualified Control.Exception
import qualified Ext.CompileMode
{-

Allows us to use `:rr` in GHCI to quickly kill, typecheck, rebuild & reboot the server,
without having to build a binary and reboot it in shell.

-}

serve = do
  projectDir <- Control.Exception.catch (Env.lookupEnv "ELM_WATCHTOWER_START_PROJECT")
                  (const $ pure Nothing ::  Control.Exception.IOException -> IO (Maybe String))
  Ext.CompileMode.setModeDisk
  trackedForkIO $
    Ext.Log.withAllBut 
      [ -- Add any flags to exclude here!
      ] $
      Watchtower.Server.serve
          projectDir
          (Watchtower.Server.Flags (Just 51213))
