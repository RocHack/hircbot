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
1. Set up Haskell:

        # or equivalents for your distribution
        yum install haskell-platform ghc-MissingH ghc-network

2. Download to directory.

3. Put settings in ~/.hircbot/cfg:

    Settings that are read (all lines must be in format "key = value"):
    - Server = IRC host name
    - Port = IRC port (must be numeric)
    - Nick = IRC nick
    - Pass = IRC server password
    - Username = IRC username
    - RealName = IRC real name field
    - Channels = comma separated list of channels to join on connect (no spaces allowed)

4. Make sure runnable:

        chmod +x HircBot.hs

5. Run it:

        ./HircBot.hs
