

module HircState
    (Host, Nick, Pass, ChannelName, Topic(..), RealName,
    Username, UserMode, ChannelMode, Server(..),
    User(..), readUser, UserPrefix(..), Channel(..), readChannel, ChannelPrefix(..),
    HircConfig(..), HircCState(..), HircState(..)) where

import System.IO (IO)
import GHC.IO.Handle (Handle)
import Network (PortNumber)

data Word = Word String

type Nick = String

type Host = String

type Username = String

type Pass = String

type ChannelName = String

type RealName = String

type UserMode = String

type ChannelMode = String

data Topic = Topic { topicText :: String
                   , topicBy :: User
                   , topicTime :: Integer }
           | NoTopic

data ChannelPrefix = Public | Private | Secret deriving Enum

instance Show ChannelPrefix where
    show Public = "="
    show Private = "*"
    show Secret = "@"

data UserPrefix = None | Op | Voice | Halfop | Admin | Owner | IsServer deriving Enum

instance Show UserPrefix where
    show Op = "@"
    show Voice = "+"
    show Halfop = "%"
    show Admin = "&"
    show Owner = "~"
    show IsServer = "*"
    show _ = ""

data User = User { nick :: Nick
                 , username :: Username
                 , host :: Host
                 , prefix :: UserPrefix }

readUser :: String -> User
readUser name@(prefix:rest)
    | name == "@" || '!' `notElem` name || '@' `notElem` name
        = User "*" "*" name IsServer -- source is not a user
    | prefix == '@' = readUser' Op rest
    | prefix == '+' = readUser' Voice rest
    | prefix == '%' = readUser' Halfop rest
    | prefix == '&' = readUser' Admin rest
    | prefix == '~' = readUser' Owner rest
    | otherwise      = readUser' None name

readUser' :: UserPrefix -> String -> User
readUser' prefix name =
    let (nick, _:upart) = break (== '!') name
        (user, _:host) = break (== '@') upart
        in User nick user host prefix

readChannel :: String -> Channel
readChannel name@(prefix:rest)
    | prefix == '=' = Channel rest NoTopic [] Public
    | prefix == '*' = Channel rest NoTopic [] Private
    | prefix == '@' = Channel rest NoTopic [] Secret

instance Show User where
    show (User _ _ h IsServer) = h
    show (User n u h p) = (show p) ++ n ++ "!" ++ u ++ "@" ++ h

data Server = Server { serverHost :: Host } deriving Show

-- Channel: represents a channel that hircbot is currently in
data Channel = Channel { channelName :: ChannelName
                       , channelTopic :: Topic
                       , channelUsers :: [User]
                       , channelPrefix :: ChannelPrefix }

instance Show Channel where
    show (Channel n t u p) = (show p) ++ n

-- HircConfig: Configuration as loaded from CL args and config file
data HircConfig = HircConfig { cfgHost :: Host
                             , cfgPort :: PortNumber
                             , cfgNick :: Nick
                             , cfgUser :: Username
                             , cfgPass :: Pass
                             , cfgRealName :: RealName
                             , cfgChans :: ChannelName }

-- HircCState: current (active) state data
-- the channels here are what the bot is currently in
data HircCState = HircCState { curSocket :: Handle
                             , curNick :: Nick
                             , curChannels :: [Channel] }
                | HircCStateDisc

-- HircState: the persistant state of Everything.
-- two sub-parts: config and current
-- config: what was loaded from the CL args and config file; when the bot connects it will use this data to initialize the current settings
-- current: what is currently true; when things happen, this will change to reflect those changes
data HircState = HircState { stateCfg :: HircConfig
                           , stateCur :: HircCState }

