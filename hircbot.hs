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

-- "enum" used to choose what type of event handle_event is handling
data EventType = OnConnect | OnReadLine | OnPipe | OnClose deriving (Enum)

-- auth state, this is sort of like a structure
data AuthState = MkAuthState Nick Pass
getASNick :: AuthState -> Nick
getASNick (MkAuthState nick _) = nick
getASPass :: AuthState -> Pass
getASPass (MkAuthState _ pass) = pass

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

-- the connection settings, this stuff will be added to
data IRCState = MkIRCState Handle AuthState
getIRCSocket :: IRCState -> Handle
getIRCSocket (MkIRCState sock _) = sock
getIRCAuth :: IRCState -> AuthState
getIRCAuth (MkIRCState _ as) = as

-- main: impure, takes cli and connects to given server
main = withSocketsDo $
        do 
           args <- getArgs
           let host:[nick:_] = args in do
               sock <- connectTo host $ PortNumber 6667
               let state = MkIRCState sock (MkAuthState "HIRCTEST" "") in do
                   send_responses state $ handle_event OnConnect state ""
                   recv_loop state

-- impure, handles receiving from socket
recv_loop :: IRCState -> IO ()
recv_loop state = do
                     line <- hGetLine $ getIRCSocket state
                     putStrLn $ "Recv: " ++ line
                     send_responses state $ handle_event OnReadLine state line
                     recv_loop state
                     return ()

-- impure, sends each given response to socket
send_responses :: IRCState -> [String] -> IO ()
send_responses state (a:b)    = let sock = getIRCSocket state in do
                                    hPutStrLn sock a
                                    putStrLn $ "Send: " ++ a
                                    send_responses state b
send_responses state []       = return ()


-- pure, takes an event and returns a list of responses to the server,
-- this is the main functionality of this client
handle_event :: EventType -> IRCState -> String -> [String]
-- on connect, send NICK
handle_event OnConnect state _ =
    [
    "NICK " ++ getASNick (getIRCAuth state),
    "USER " ++ getASNick (getIRCAuth state) ++ " 0 * :Nate",
    "JOIN #rochack"
    ]

-- unhandled events return no output
handle_event _ _ _ = []

