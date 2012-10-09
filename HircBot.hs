#!/usr/bin/runhaskell
-- HIRC v0.1
-- Nate Book

module HircBot where

-- Haskell modules
import System.Environment

-- HIrcBot modules
import HircIO -- logging, config, network
import HircState -- HircState object and all types
import HircEvents -- What to do on abstracted IRC events

main =
    do
        ircNetInit
        args <- getArgs
        let argMap = parseCliArgs args
            in do
                state <- loadConfig
                logLine state LogInfo "Welcome to HircBot. Configuration file loaded."
                state <- handleCliArgs argMap state
                state <- ircConnect state
                logLine state LogInfo $ "Connected to " ++ (cfgHost . stateCfg $ state) ++ "."
                state <- handleEvent state OnConnect
                ircRecvLoop state handleMessage
                where
                    handleMessage :: HircState -> Message -> IO HircState
                    handleMessage state m = handleEvent state $ OnRecv m


