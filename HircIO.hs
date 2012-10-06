

module HircIO
    (CliFlag(..), parseCliArgs,
    LogLevel(..), logLine,
    loadConfig,
    Message(..), MOrigin, MCommand, MCommandArgs,
    ircNetInit, ircConnect, ircSend, ircRecvLoop, readMsg) where

-- Haskell modules
import Network
import GHC.IO.Handle
import System.IO
import System.Posix.Signals

import HircState

instance Show Message where
    show (Message "" c a) = c ++ (showArgs a)
    show (Message p c a) = ":" ++ p ++ " " ++ c ++ (showArgs a)

showArgs :: MCommandArgs -> String
showArgs (a:arest) | contains ' ' a = " :" ++ a
                   | otherwise = " " ++ a ++ (showArgs arest)
showArgs [] = ""

contains :: Eq a => a -> [a] -> Bool
contains item (h:t) | h == item = True
                    | otherwise = contains item t
contains item [] = False

readMsg :: String -> Message
readMsg s = Message p c a
            where
                (p:rest) = splitIRCLn s
                (c:a) = if rest == []
                    then [""]
                    else rest

-- split something of format [:prefix] <command> [arg0 arg1 ... argn] [:argrest]
-- into [prefix, command, arg0, arg1, ... argn, argrest]
splitIRCLn :: String -> [String]
splitIRCLn "" = [""]
splitIRCLn line =
    -- does the line start with : (if so prefix is present)
    let chr1:_ = line in
        if chr1 == ':'
        then let (prefix, command) = break (== ' ') line in
            -- use prefix and parse rest of list
            (drop 1 prefix) : (drop 1 . splitIRCLn . drop 1 $ command)
        else let (command, args) = break (== ' ') line in
            if length args <= 1
            then "" : [command]
            else if (head . drop 1 $ args) == ':'
                then let rest = drop 2 args in
                    "" : [command, rest]
                else "" : (command : (drop 1 . splitIRCLn . drop 1 $ args))


type MOrigin = String
type MCommand = String
type MCommandArgs = [String]

-- Message: represents a raw message sent over the network
-- MSource is the prefix or source used/to use, where "" means no source was given (nearest IRC node)
data Message = Message {
    msgOrigin :: MOrigin,
    msgCommand :: MCommand,
    msgCommandArgs :: MCommandArgs }

-- CliFlags: possible options in CL arguments
data CliFlag = CliVerbose | CliServer | CliPort | CliNick deriving Enum

-- LogLevel: levels logging can occur at
data LogLevel = LogError | LogWarning | LogInfo | LogDebug deriving Enum

cliMap = [ ("v0O", CliVerbose),
           ("s1R", CliServer),
           ("p1O", CliPort),
           ("n1R", CliNick) ]

-- parses CLI args and returns a map of flag -> value (can be "")
parseCliArgs :: [String] -> [(CliFlag, String)]
parseCliArgs args= []

-- loads configuration from the given file name and creates a HircState
loadConfig :: String -> HircState
loadConfig s = HircState
    (HircConfig "irc.freenode.net" 6667 "hircbot" "hircbot" "" "HircBot v0.2 -- nmbook")
    HircCStateDisc

ircNetInit :: IO ()
ircNetInit =
    withSocketsDo $ do
        installHandler sigPIPE Ignore Nothing
        return ()

-- connects to IRC
ircConnect :: HircState -> IO HircState
ircConnect state =
    do
        sock <- connectTo (cfgHost . stateCfg $ state) (PortNumber . cfgPort . stateCfg $ state)--(stateCfgGetServerPort state))
        hSetBuffering sock LineBuffering
        return $ HircState (stateCfg state) (HircCState sock (cfgNick . stateCfg $ state) [])

-- receive loop for IRC
ircRecvLoop :: HircState -> (HircState -> Message -> IO HircState) -> IO ()
ircRecvLoop state handleFn =
    do
        line <- hGetLine $ curSocket . stateCur $ state
        state <- handleFn state (readMsg line)
        ircRecvLoop state handleFn
        return ()

-- send for IRC
ircSend :: HircState -> Message -> IO ()
ircSend state msg = hPutStrLn (curSocket . stateCur $ state) (show msg)

-- logs a line to the log file
logLine :: HircState -> LogLevel -> String -> IO ()
logLine state lv ln = return ()

