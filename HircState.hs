

module HircState
    (Host, Nick, Pass, ChannelName, Topic(..), RealName,
    Username, UserMode, ChannelMode, Server(..),
    User(..), readUser, UserPrefix(..), Channel(..), readChannel, ChannelPrefix(..),
    HircConfig(..), HircCState(..), HircState(..)) where

import List (find)
import Data.List (isPrefixOf)
import System.IO (IO)
import GHC.IO.Handle (Handle)
import Network (PortNumber, PortID)

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

data User = User { userNick :: Nick
                 , userName :: Username
                 , userHost :: Host
                 , userPrefix :: UserPrefix }

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
--data HircConfig = HircConfig { cfgHost :: Host
--                             , cfgPort :: PortNumber
--                             , cfgNick :: Nick
--                             , cfgUser :: Username
--                             , cfgPass :: Pass
--                             , cfgRealName :: RealName
--                             , cfgChans :: ChannelName }

-- HircCState: current (active) state data
-- the channels here are what the bot is currently in
--data HircCState = HircCState { curSocket :: Handle
--                             , curNick :: Nick
--                             , curChannels :: [Channel] }
--                | HircCStateDisc

-- HircState: the persistant state of Everything.
-- two sub-parts: config and current
-- config: what was loaded from the CL args and config file; when the bot connects it will use this data to initialize the current settings
-- current: what is currently true; when things happen, this will change to reflect those changes
--data HircState = HircState { stateCfg :: HircConfig
--                           , stateCur :: HircCState }





-- A HircState is a map of state data:
-- group, key, value

-- group name
data HircStateKeyGroup = Cfg | Cur | Usr | Chn deriving (Show,Read,Eq,Enum)

-- key name
type HircStateKey = String

-- values: typed values
data HircStateValue = HostValue { host :: NetworkHost }
                    | NameValue { name :: String }
                    | PassValue { pass :: String }
                    | ChannelNamesValue { channelNames :: [String] }
                    | HandlesValue { handles :: [NamedHandle] }
                    | ChannelsValue { channels :: [Channel] }
                    | ChannelValue { c :: Channel }
                    | UsersValue { users :: [User] }
                    | UserValue { u :: User }
                    | NullValue

-- actual data:
--   cfg.Server = NetworkHost
--   cfg.Nick = Name
--   cfg.Username = Name
--   cfg.Realname = Realname
--   cfg.Pass = Pass
--   cfg.Channels = ChannelNames
--
--   cur.Handles = Handles
--   cur.Channels = Channels
--   cur.Users = Users
--   cur.Nick = Name
--   cur.Username = Name
--   cur.Realname = Realname
--   cur.Pass = Pass
--
--   usr.[name] = User
--   chn.[name] = Channel
type HircStateRecord = (HircStateKeyGroup, HircStateKey, HircStateValue)

newtype HircState = HircState [HircStateRecord]

sGrp :: HircStateRecord -> HircStateKeyGroup
sGrp (grp,_,_) = grp

sKey :: HircStateRecord -> HircStateKey
sKey (_,key,_) = key

sVal :: HircStateRecord -> HircStateValue
sVal (_,_,val) = val

sGet :: HircState -> HircStateKeyGroup -> HircStateKey -> Maybe HircStateValue
sGet = fmap sVal sGetRecord

sMatchRecord :: HircStateKey -> HircStateValue -> HircStateRecord -> Bool
sMatchRecord grp key (rgrp, rkey, _) = rgrp == grp && rkey == key

sGetRecord :: HircState -> HircStateKeyGroup -> HircStateKey -> Maybe HircStateRecord
sGetRecord state grp key = find (sMatchRecord grp key) state

sPut :: HircState -> HircStateKeyGroup -> HircStateKey -> HircStateValue -> HircState
sPut state grp key val = sPutRecord state (grp, key, val)

sPutRecord :: HircState -> HircStateRecord -> HircState
sPutRecord state rec = [rec] ++ (sDelRecord rec)

sDelRecord :: HircState -> HircStateRecord -> HircState
sDelRecord state (grp, key, _) = filter (not . sMatchRecord $ grp key) state

sDel :: HircState -> HircStateKeyGroup -> HircStateKey -> HircState
sDel state grp key = sDelRecord state $ sGetRecord grp key

-- types available:

-- NamedHandle, allows you to name sockets stored in the state
-- NamedHandle with the name "irc:<server>" is the irc server given
-- NamedHandle with the name "log:<date>" is the current log file
-- NamedHandle with the name 
-- other NamedHandle can be created for other purposes (such as HTTP requests, etc), must 
data NamedHandle = NamedHandle { hName :: String, h :: Handle }

handleByName :: [NamedHandle] -> String -> Maybe Handle
handleByName list name = fmap h $ find ((== name) . hName) list

handleByNameStartsWith :: [NamedHandle] -> String -> [NamedHandle]
handleByNameStartsWith list nameSW = filter ((isPrefixOf nameSW) . hName) list

putHandle :: [NamedHandle] -> String -> Handle -> [NamedHandle]
putHandle list name handle = [NamedHandle name handle] ++ (filter ((/= name) . hName) list)

-- NetworkHost, a server and port
data NetworkHost = NetworkHost { domain :: String, port :: PortID }



