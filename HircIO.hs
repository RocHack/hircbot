

module HircIO
    (CliFlag(..), parseCliArgs, handleCliArgs,
    LogLevel(..), logLine,
    loadConfig,
    Message(..), MOrigin, MCommand, MCommandArgs, Numeric, toNumeric,
    ircNetInit, ircConnect, ircSend, ircRecvLoop, readMsg) where

-- Haskell modules
import Data.Char
import Data.List
import Data.Text (pack, strip, unpack)
import Data.Time
import GHC.IO.Handle
import Network
import System.Directory
import System.IO
import System.Locale
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
readMsg s = Message p c a where
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
    let chr1:_ = line
    in if chr1 == ':'
        then let (prefix, command) = break (== ' ') line
            in
            -- use prefix and parse rest of list
            (drop 1 prefix) : (drop 1 . splitIRCLn . drop 1 $ command)
        else let (command, args) = break (== ' ') line
            in if length args <= 1
                then "" : [command]
                else if (head . drop 1 $ args) == ':'
                    then let rest = drop 2 args
                        in  "" : [command, rest]
                    else    "" : (command : (drop 1 . splitIRCLn . drop 1 $ args))


type MOrigin = String
type MCommand = String
type MCommandArgs = [String]

-- Message: represents a raw message sent over the network
-- MSource is the prefix or source used/to use, where "" means no source was given (nearest IRC node)
data Message = Message {
    msgOrigin :: MOrigin,
    msgCommand :: MCommand,
    msgCommandArgs :: MCommandArgs }

-- an IRC Numeric (3 digit number)
type Numeric = Int

-- converting from string
toNumeric :: String -> Numeric
toNumeric s = foldl ((+) . (* 10)) 0 $ map digitToInt s

-- LogLevel: levels logging can occur at
data LogLevel = LogError | LogWarning | LogInfo | LogDebug | LogVerbose deriving Enum
instance Show LogLevel where
    show LogError = " ERROR"
    show LogWarning = " WARNING"
    show LogInfo = " INFO"
    show LogDebug = " DEBUG"
    show _ = " VERBOSE"

-- TODO: no CLI arguments actually handled at this time
-- CliFlags: possible options in CL arguments
data CliFlag = CliVerbose | CliServer | CliPort | CliNick deriving Enum

-- parses CLI args and returns a map of flag -> value (can be "")
parseCliArgs :: [String] -> [(CliFlag, String)]
parseCliArgs args = []

handleCliArgs :: [(CliFlag, String)] -> HircState -> IO HircState
handleCliArgs ((key, val):rest) state =
    handleCliArgs rest state -- TODO: currently ignores CLI args
handleCliArgs [] state = return state

cfgFolder :: IO FilePath
cfgFolder =
    do
        homeDir <- getHomeDirectory
        let cfgDir = homeDir ++ "/.hircbot"
            in do
                createDirectoryIfMissing False cfgDir
                return cfgDir

-- loads configuration from the given file name and creates a HircState
loadConfig :: IO HircState
loadConfig =
    do
        cfgDir <- cfgFolder
        fileExist <- doesFileExist $ cfgDir ++ "/cfg"
        lines <- if fileExist
            then do
                h <- openFile (cfgDir ++ "/cfg") ReadMode
                loadConfigLn h
            else return []
        let server = byKey "Server" lines "irc.freenode.net"
            port = byKey "Port" lines "6667"
            nick = byKey "Nick" lines ""
            username = byKey "Username" lines ""
            pass = byKey "Pass" lines ""
            rname = byKey "RealName" lines "HircBot v0.2 -- nmbook"
            chans = byKey "Channels" lines "#hircbot"
            in do
                return $ HircState (HircConfig server (toEnum (read port :: Int)) nick username pass rname chans) HircCStateDisc
    where
        byKey :: String -> [(String, String)] -> String -> String
        byKey key [] def = def
        byKey key ((isKey, val):list) def =
            if isKey == key
                then val
                else byKey key list def
        loadConfigLn :: Handle -> IO [(String, String)]
        loadConfigLn handle = loadConfigLn' handle []
        loadConfigLn' :: Handle -> [(String, String)] -> IO [(String, String)]
        loadConfigLn' handle part =
            do
                isEof <- hIsEOF handle
                if isEof
                    then do
                        hClose handle
                        return part
                    else do
                        line <- hGetLine handle
                        if any (== '=') line
                            then let (key, val) = break (== '=') line
                                in loadConfigLn' handle
                                        ((unpack . strip . pack $ key,
                                        unpack . strip . pack . drop 1 $ val) : part)
                            else loadConfigLn' handle part

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
        state <- if last line == '\r'
            then handleFn state . readMsg . init $ line
            else handleFn state . readMsg $ line
        ircRecvLoop state handleFn
        return ()

-- send for IRC
ircSend :: HircState -> Message -> IO ()
ircSend state msg = hPutStr (curSocket . stateCur $ state) ((show msg) ++ "\r\n")

-- logs a line to the log file
logLine :: HircState -> LogLevel -> String -> IO ()
logLine state lv ln =
    do
        now <- getZonedTime
        fp <- getCurrentLogFile now
        let output = "[" ++ (getCurrentTimeStamp now) ++ "]" ++ (show lv) ++ ": " ++ ln ++ "\n"
            in do
                appendFile fp output
                putStr output

getCurrentTimeStamp :: ZonedTime -> String
getCurrentTimeStamp = formatTime defaultTimeLocale "%I:%M:%S %p"

getCurrentLogFile :: ZonedTime -> IO FilePath
getCurrentLogFile now =
    do
        cfgDir <- cfgFolder
        let logDir = cfgDir ++ "/logs"
            in do
                createDirectoryIfMissing False logDir
                return $ logDir ++ "/" ++ (formatTime defaultTimeLocale "%Y-%m-%d" now) ++ ".txt"

