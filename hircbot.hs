#!/usr/bin/runhaskell
-- HIRC v0.1
-- Nate Book

import Network
import GHC.IO.Handle
import System.Environment
import System.IO

type Pass = String
type Nick = String
type ChannelName = String
type Flags = Int
type Source = String
type Command = String
type CommandArgs = [String]

data Message = MkMessage Source Command CommandArgs
getMSource :: Message -> Source
getMSource (MkMessage s _ _) = s
getMCommand :: Message -> Command
getMCommand (MkMessage _ c _) = c
getMArgs :: Message -> CommandArgs
getMArgs (MkMessage _ _ a) = a

instance Show Message where
    show (MkMessage "" cmd args) = cmd ++ showArgs args
    show (MkMessage prefix cmd args) = ":" ++ prefix ++ " " ++ cmd ++ showArgs args

--instance Show CommandArgs where
showArgs :: CommandArgs -> String
--showArgs [""] = ""
showArgs (arg1:argrest) = if (length $ words arg1) == 1
                          then " " ++ arg1 ++ showArgs argrest
                          else " :" ++ arg1
showArgs _ = ""

-- "enum" used to choose what type of event handle_event is handling
data EventType = OnConnect | OnReadLine | OnPipe | OnClose deriving (Enum)

-- auth state, this is sort of like a structure
data AuthState = MkAuthState Nick Pass
getASNick :: AuthState -> Nick
getASNick (MkAuthState nick _) = nick
getASPass :: AuthState -> Pass
getASPass (MkAuthState _ pass) = pass

-- the connection settings, this stuff will be added to
data IRCState = MkIRCState Handle AuthState
getIRCSocket :: IRCState -> Handle
getIRCSocket (MkIRCState sock _) = sock
getIRCAuth :: IRCState -> AuthState
getIRCAuth (MkIRCState _ as) = as

data EventReturn = MkReturn [Message] IRCState
getRetMsgs :: EventReturn -> [Message]
getRetMsgs (MkReturn m _) = m
getRetState :: EventReturn -> IRCState
getRetState (MkReturn _ s) = s

-- user data type
data User = MkUser Nick Flags
getUNick :: User -> Nick
getUNick (MkUser nick _) = nick
getUFlags :: User -> Flags
getUFlags (MkUser _ flags) = flags

-- channel data type
data Channel = MkChannel ChannelName [User]
getChName :: Channel -> ChannelName
getChName (MkChannel name _) = name
getChUsers :: Channel -> [User]
getChUsers (MkChannel _ users) = users

-- main: impure, takes cli and connects to given server
main = withSocketsDo $
    do 
       args <- getArgs
       let host:(nick:_) = args in do
           sock <- connectTo host $ PortNumber 6667
           let state = MkIRCState sock (MkAuthState nick "") in
               let ret = handle_event OnConnect state "" in do
                   send_responses (getRetState ret) (getRetMsgs ret)
                   recv_loop $ getRetState ret

-- impure, handles receiving from socket
recv_loop :: IRCState -> IO ()
recv_loop state =
    do
       line <- hGetLine $ getIRCSocket state
       putStrLn $ "Recv: " ++ line
       let ret = handle_event OnReadLine state line in do
           send_responses (getRetState ret) (getRetMsgs ret)
           recv_loop $ getRetState ret

-- impure, sends each given response to socket
send_responses :: IRCState -> [Message] -> IO ()
send_responses state (a:b)    = let sock = getIRCSocket state in do
                                    hPutStrLn sock $ show a
                                    putStrLn $ "Send: " ++ (show a)
                                    send_responses state b
send_responses state []       = return ()


-- pure, takes an event and returns a list of responses to the server,
-- this is the main functionality of this client
handle_event :: EventType -> IRCState -> String -> EventReturn
-- on connect, send NICK
handle_event OnConnect state _ =
    MkReturn 
    [
    MkMessage "" "NICK" [getASNick $ getIRCAuth state],
    MkMessage "" "USER" [getASNick $ getIRCAuth state, "0", "*", ":hircbot"],
    MkMessage "" "JOIN" ["##rochack"]
    ]
    state

-- handle server command
handle_event OnReadLine state line =
    handle_line state $ splitIRCLn line

-- unhandled events return no output
handle_event _ state _ = MkReturn [] state

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

-- handle server line
handle_line :: IRCState -> [String] -> EventReturn
handle_line state (source:(command:args)) =
    handle_line_2 state source command args

handle_line_2 :: IRCState -> String -> String -> [String] -> EventReturn
handle_line_2 state _ "PING" args = MkReturn [MkMessage "" "PONG" []] state
handle_line_2 state _ _ _ = MkReturn [] state

