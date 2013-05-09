steel overseer
==============
A tool that runs a command after files change on disk. 
A file watcher.

<img src="https://raw.github.com/schell/steeloverseer/master/rsrc/pic.jpg" width="300" title="steel overseer" />

installation
------------
After cloning and cd'ing into the repo run ```cabal install```, which should install the ```sos``` executable in your $HOME/.cabal/bin.

usage
-----

    sos: usage: sos [vV] [iI] cC [file extensions...]
      -v, -V                  --version          show version number
      -c command, -C command  --command=command  command to run on change

Example:
    > sos -c "cabal install" hs cabal md
    
Immediately results in:
    
    Starting Steel Overseer to perform "cabal install" when files of type hs, cabal, md change in the current directory (./)

And after ```touch src/*``` is run from my vim:

    > cabal install
    
    Running process after events: 
    Modified (FilePath "/Users/schell/Code/steeloverseer/src/SOS.hs") 2013-05-08 22:23:17.648902 UTC
    Modified (FilePath "/Users/schell/Code/steeloverseer/src/Main.hs") 2013-05-08 22:23:17.648787 UTC
    Modified (FilePath "/Users/schell/Code/steeloverseer/src/ANSIColors.hs") 2013-05-08 22:23:17.648071 UTC
    Resolving dependencies...
    Configuring steeloverseer-0.1.0.1...
    Building steeloverseer-0.1.0.1...
    Preprocessing executable 'sos' for steeloverseer-0.1.0.1...
    [1 of 3] Compiling ANSIColors       ( src/ANSIColors.hs, dist/build/sos/sos-tmp/ANSIColors.o )
    [2 of 3] Compiling SOS              ( src/SOS.hs, dist/build/sos/sos-tmp/SOS.o )
    [3 of 3] Compiling Main             ( src/Main.hs, dist/build/sos/sos-tmp/Main.o )
    Linking dist/build/sos/sos ...
    Installing executable(s) in /Users/schell/.cabal/bin

plus
----
It has (some) color!
