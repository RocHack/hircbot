

module HircState
    (Host, Nick, Pass, ChannelName, Topic, RealName,
    NSName, NSPass, Username,
    Entity(..), Channel(..), Flags(..), HircConfig(..), HircCState(..), HircState(..)) where

import System.IO (IO)
import GHC.IO.Handle (Handle)
import Network (PortNumber)

data Word = Word String

type Nick = String

type Host = String

type Username = String

type Pass = String

type ChannelName = String

type Topic = String

type RealName = String

type NSName = String

type NSPass = String

type UserFlag = Char

--data UserFlag =

data Entity = User { nick :: Nick
                   , username :: Username
                   , host :: Host
                   , flags :: [UserFlag] }
            | Server { serverHost :: Host }

instance Show Entity where
    show (Server h) = h
    show (User n u h _) = n ++ "!" ++ u ++ "@" ++ h

-- Channel: represents a channel that hircbot is currently in
data Channel = Channel { channelName :: ChannelName
                       , channelTopic :: Topic
                       , channelUsers :: [Entity] }

-- Flags: User flags (TODO: I'll add the correct list later)
data Flags = Voice | Op deriving Enum

-- HircConfig: Configuration as loaded from CL args and config file
data HircConfig = HircConfig { cfgHost :: Host
                             , cfgPort :: PortNumber
                             , cfgNick :: Nick
                             , cfgUser :: Username
                             , cfgPass :: Pass
                             , cfgRealName :: RealName }

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

