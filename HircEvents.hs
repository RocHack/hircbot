
module HircEvents
                 (EventType(..), Command(..), Source(..), CName, CArg,
                 handleEvent) where

import HircState
import HircIO

import Data.Char
import Data.List.Split
import System.IO

data Source = ChannelSource ChannelName User
             | PrivateSource User
             | InternalSource

-- makes a Source object given the origin (IRC Message Prefix) and the destination (PRIVMSG/NOTICE argument 1)
mkSource :: User -> String -> Source
mkSource origin dest@('#':_) = ChannelSource dest origin
mkSource origin _ = PrivateSource origin

-- gets the user/location to reply to in PRIVMSG/NOTICE argument 1 given the source
replyTo :: Source -> String
replyTo (ChannelSource c _) = c
replyTo (PrivateSource u) = nick u
replyTo _ = ""

-- gets the user who sent the message regardless of where the message is going
sender :: Source -> User
sender (ChannelSource _ u) = u
sender (PrivateSource u) = u

type ChatMsg = String
type CName = String
type CArg = String

data Chat = Chat Source ChatMsg

data Command = Command Source CName [CArg]

readCmd :: Source -> ChatMsg -> Command
readCmd s "" = Command s "" []
readCmd s m =
    let (cword:args) = split (dropDelims $ whenElt (== ' ')) m
        in Command s cword args

data EventType = OnConnect
               | OnDisconnect
               | OnPing
               | OnSent Message
               | OnRecv Message
               | OnRecvNumeric Numeric Message
               | OnServerMessage String
               | OnSentChat Chat
               | OnRecvChat Chat
               | OnCommand Command
               | OnChannel Channel
               | OnTopic Topic
               | OnUsers [User]
               | OnUserJoin User
               | OnUserPart User
               | OnUserNick User Nick
               | OnUserMode User

ircSend' :: HircState -> Message -> IO HircState
ircSend' state m =
    do
        ircSend state m
        handleEvent state (OnSent m)

respond :: HircState -> Source -> String -> IO HircState
respond state src msg =
    do
        logLine state LogInfo $ "[" ++ (replyTo src) ++ "] (ME) " ++ (curNick . stateCur $ state) ++ ": " ++ msg
        ircSend' state $ Message "" "PRIVMSG" [replyTo src, msg]

handleEvent :: HircState -> EventType -> IO HircState

-- ONCONNECT EVENT:: Occurs after you have connected
handleEvent state OnConnect =
    do
        state <- if (cfgPass . stateCfg $ state) == ""
            then return state
            else ircSend' state $ Message "" "PASS" [cfgPass . stateCfg $ state]
        state <- ircSend' state $ Message "" "NICK" [cfgNick . stateCfg $ state]
        state <- ircSend' state $
            Message "" "USER" [cfgUser . stateCfg $ state, "0", "*", cfgRealName . stateCfg $ state]
        state <- if (cfgChans . stateCfg $ state) == ""
            then return state
            else ircSend' state $ Message "" "JOIN" [cfgChans . stateCfg $ state]
        return state

-- ONRECV a numeric, calls ONRECVNUMERIC
handleEvent state (OnRecv msg@(Message src cmd@[c1,c2,c3] args))
    | isDigit c1 && isDigit c2 && isDigit c3
        = handleEvent state $ OnRecvNumeric (toNumeric cmd) msg
    | otherwise
        = handleEvent state $ OnRecv $ Message src (cmd ++ " ") args

-- ONRECV
handleEvent state (OnRecv (Message src cmd args))
    | cmd == "PING"
        = handleEvent state OnPing
    | cmd == "PRIVMSG"
        = let user = readUser src
            in do
                logLine state LogInfo $ "[" ++ (args !! 0) ++ "] " ++ (nick user) ++ ": " ++ (args !! 1)
                handleEvent state $ OnRecvChat $ Chat (mkSource user $ args !! 0) (args !! 1)
    | cmd == "NOTICE"
        = let user = readUser src
            in do
                logLine state LogInfo $ "[" ++ (args !! 0) ++ "] (NOTICE) " ++ (nick user) ++ ": " ++ (args !! 1)
                return state
    | cmd == "JOIN"
        = let user = readUser src
            in do
                logLine state LogInfo $ "[" ++ (args !! 0) ++ "] Join: " ++ (nick user)
                return state
    | cmd == "PART"
        = let user = readUser src
            in do
                logLine state LogInfo $ "[" ++ (args !! 0) ++ "] Part: " ++ (nick user) ++ " (" ++ (args !! 1) ++ ")"
                return state
    | cmd == "TOPIC"
        = let user = readUser src
            in do
                logLine state LogInfo $ "[" ++ (args !! 0) ++ "] Topic by " ++ (nick user) ++ ": " ++ (args !! 1)
                return state
    | cmd == "QUIT"
        = let user = readUser src
            in do
                logLine state LogInfo $ "Quit: " ++ (nick user) ++ " (" ++ (args !! 0) ++ ")"
                return state
    | cmd == "MODE"
        = let user = readUser src
            in do
                logLine state LogInfo $ "Mode: " ++ (nick user) ++ " " ++ (args !! 1) ++ " by " ++ (nick user)
                return state
    | otherwise
        = do
            logLine state LogWarning $ "Unsupported message " ++ cmd ++ ". Arguments: " ++ (show args)
            return state

-- ONRECV numerics
handleEvent state (OnRecvNumeric num (Message origin _ (target:args)))
    | any (== num) [001,002,003,250,251,255] -- RPL_WELCOME, RPL_YOURHOST, RPL_CREATED, RPL_STATSCONN, RPL_LUSERCLIENT, RPL_LUSERME
        = handleEvent state $ OnServerMessage $ head args
    | num == 004 -- RPL_MYINFO
        = do
            logLine state LogInfo $ "Connected to: " ++ (args !! 0) ++ "  Server version: " ++ (args !! 1)
            logLine state LogInfo $ "User Modes: " ++ (args !! 2)
            logLine state LogInfo $ "Channel Modes: " ++ (args !! 3)
            return state
    | num == 005 -- RPL_ISUPPORT
        = do
            --spam!
            --logLine state LogInfo $ "Supported Features: " ++ (show args)
            return state
    | num == 252 -- RPL_LUSEROP
        = do
            -- TODO: number of IRC operators online
            return state
    | num == 253 -- RPL_LUSERUNKNOWN
        = do
            -- TODO: number of unknown connections
            return state
    | num == 254 -- RPL_LUSERCHANNELS
        = do
            -- TODO: number of channels
            return state
    | num == 265 -- RPL_LOCALUSERS
        = do
            -- TODO: number of users on THIS server, arg !! 0 = cur, arg !! 1 = max
            return state
    | num == 266 -- RPL_GLOBALUSERS
        = do
            -- TODO: number of users on network, arg !! 0 = cur, arg !! 1 = max
            return state
    | num == 332 -- RPL_TOPIC
        = do
            logLine state LogInfo $ "Topic for " ++ (head args) ++ ": " ++ (args !! 1)
            return state
    | num == 333 -- RPL_TOPICWHOTIME
        = do
            logLine state LogInfo $ "Topic for " ++ (head args) ++ " by: " ++ (nick (readUser (args !! 1))) ++ " at (TODO: unix time) " ++ (args !! 2)
            return state
    | num == 353 -- RPL_NAMREPLY
        = do
            logLine state LogInfo $ "Users in channel " ++ (head args) ++ (args !! 1) ++ ": " ++ (args !! 2)
            return state
    | num == 366 -- RPL_ENDOFNAMES
        = do
            -- TODO: names end
            return state
    | num == 372 -- RPL_MOTD
        = do
            -- TODO: motd line
            return state
    | num == 375 -- RPL_MOTDSTART
        = do
            -- TODO: motd header
            return state
    | num == 376 -- RPL_ENDOFMOTD
        = do
            -- TODO: motd end
            return state
    | otherwise
        = do
            logLine state LogWarning $ "Unsupported numeric " ++ (show num) ++ ". Arguments: " ++ (show args)
            return state

-- on any generic server info message
handleEvent state (OnServerMessage msg) =
    do
        logLine state LogInfo $ "Server: " ++ msg
        return state

handleEvent state (OnRecvChat (Chat src msg)) =
    let word1 = fst . break (== ' ') $ msg
        in if word1 == "!h" || word1 == "!hb" || word1 == "!hirc" || word1 == "!hircbot"
            then handleEvent state $ OnCommand $ readCmd src $ drop 1 . snd . break (== ' ') $ msg
            else return state

handleEvent state (OnCommand (Command src cword args)) =
    case cword of
        "say"
            -> respond state src $ unwords args
        "about"
            -> respond state src $ "I am HircBot, written by nmbook."
        "whoami"
            -> respond state src $ "You are " ++ (nick . sender $ src) ++ ". Username: " ++ (username . sender $ src) ++ " Host: " ++ (host . sender $ src)
        otherwise
            -> respond state src $ "Command '" ++ cword ++ "' was not known."

handleEvent state OnPing = ircSend' state $ Message "" "PONG" []

handleEvent state (OnSent line) =
    do
        return state

-- required to match unused events
handleEvent state _ = return state

