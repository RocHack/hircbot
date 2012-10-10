hircbot
=======

IRC Bot in Haskell

By [RocHack](http://rochack.github.com/)

Authors
-------
- [nmbook](http://github.com/nmbook/)

Features
--------
- Reads configuration in ~/.hircbot/cfg
- Writes logs to stdout and ~/.hircbot/YYYY-MM-DD.txt
- Connects to IRC to the set of channels specified.
- Responds to commands starting with !h or !hircbot:
    - say: repeats input
    - about: prints HircBot by nmbook
    - whoami: prints your nick, username, host

To Use
------
1. Make sure you have required Haskell libraries:

        cabal install network
        cabal install split

2. Download to directory.

4. Put settings in ~/.hircbot/cfg:

    Settings that are read (all lines must be in format "key = value"):
    - Server = IRC host name
    - Port = IRC port (must be numeric)
    - Nick = IRC nick
    - Pass = IRC server password
    - Username = IRC username
    - RealName = IRC real name field
    - Channels = comma separated list of channels to join on connect (no spaces allowed)

3. Make sure runnable:

        chmod +x HircBot.hs

5. Run it:

        ./HircBot.hs
