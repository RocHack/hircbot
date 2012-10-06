
module HircEvents
                 (EventType(..), Command(..), Source(..), CName, CArg,
                 handleEvent) where

import HircState
import HircIO

import System.IO

data Source = ChannelSource ChannelName String
             | PrivateSource String
             | InternalSource

-- makes a Source object given the origin (IRC Message Prefix) and the destination (PRIVMSG/NOTICE argument 1)
mkSource :: String -> String -> Source
mkSource origin dest@('#':_) = ChannelSource dest origin
mkSource origin _ = PrivateSource origin

-- gets the user/location to reply to in PRIVMSG/NOTICE argument 1 given the source
replyTo :: Source -> String
replyTo (ChannelSource c _) = c
replyTo (PrivateSource u) = u
replyTo _ = ""

-- gets the user who sent the message regardless of where the message is going
sender :: Source -> String
sender (ChannelSource _ u) = u
sender (PrivateSource u) = u
sender _ = ""

type ChatMsg = String
type CName = String
type CArg = String

data Chat = Chat Source ChatMsg

data Command = Command Source CName [CArg]

readCmd :: Source -> ChatMsg -> Command
readCmd s "" = Command s "" []
readCmd s m =
    let (cword:args) = words m in
        Command s cword args

data EventType = OnConnect
               | OnDisconnect
               | OnPing
               | OnSent Message
               | OnRecv Message
               | OnSentChat Chat
               | OnRecvChat Chat
               | OnCommand Command
               | OnChannel ChannelName Topic [Entity] -- ChannelFlags
               | OnUserJoin Entity
               | OnUserPart Entity
               | OnUserNick Entity Nick
               | OnUserMode Entity -- UserFlags

ircSend' :: HircState -> Message -> IO HircState
ircSend' state m =
    do
        ircSend state m
        handleEvent state (OnSent m)

respond :: HircState -> Source -> String -> IO HircState
respond state src msg = ircSend' state $ Message "" "PRIVMSG" [replyTo src, msg]

handleEvent :: HircState -> EventType -> IO HircState

-- ONCONNECT EVENT:: Occurs after you have connected
handleEvent state OnConnect =
    do
        if (cfgPass . stateCfg $ state) == ""
            then return ()
            else ircSend state $ Message "" "PASS" [cfgPass . stateCfg $ state]
        state <- ircSend' state $ Message "" "NICK" [cfgNick . stateCfg $ state]
        state <- ircSend' state $
            Message "" "USER" [cfgUser . stateCfg $ state, "0", "*", cfgRealName . stateCfg $ state]
        state <- ircSend' state $ Message "" "JOIN" ["##rochack"]
        return state

handleEvent state (OnRecv line) =
    do
        --putStrLn $ "R: " ++ (show line)
        state <- case line of
            (Message _ "PING" _)
                -> handleEvent state OnPing
            (Message src "PRIVMSG" (loc:(msg:_)))
                -> handleEvent state $ OnRecvChat $ Chat (mkSource src loc) msg
            otherwise
                -> return state
        return state

handleEvent state (OnRecvChat (Chat src msg)) =
    let word1 = head . words $ msg in if word1 == "!h" || word1 == "!hb" || word1 == "!hirc" || word1 == "!hircbot"
        then handleEvent state $ OnCommand $ readCmd src $ unwords . drop 1 . words $ msg
        else return state
        --ircSend' state $ Message "" "PRIVMSG" [replyTo s, unwords . drop 1 . words $ msg]
        --"!hbsend" ->
        --    ircSend' state $ readMsg . unwords . drop 1 . words $ msg
        --otherwise ->
        --    return state

handleEvent state (OnCommand (Command src cword args)) =
    case cword of
        "say"
            -> respond state src $ unwords args
        "about"
            -> respond state src $ "I am HircBot, written by nmbook."
        "whoami"
            -> respond state src $ "You are " ++ nick ++ ". Username: " ++ username ++ " Host: " ++ host
            where
                (nick,_:uh) = break (=='!') (sender src)
                (username,_:host) = break (=='@') uh
        otherwise
            -> respond state src $ "Command '" ++ cword ++ "' was not known."

handleEvent state OnPing = ircSend' state $ Message "" "PONG" []

handleEvent state (OnSent line) =
    do
        --putStrLn $ "S: " ++ (show line)
        return state

-- required to match unused events
handleEvent state _ = return state
