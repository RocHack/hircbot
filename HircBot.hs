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
            state = loadConfig "hircbot.ini" in
                handleCliArgs argMap state

handleCliArgs :: [(CliFlag, String)] -> HircState -> IO ()
handleCliArgs ((key, val):rest) state =
    handleCliArgs rest state -- TODO: currently ignores CLI args
handleCliArgs [] state =
    do
        state <- ircConnect state
        state <- handleEvent state OnConnect
        ircRecvLoop state handleMessage
        where
            handleMessage :: HircState -> Message -> IO HircState
            handleMessage state m = handleEvent state (OnRecv m)

