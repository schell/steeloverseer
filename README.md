steel overseer
==============
A tool that runs a list of commands after files change on disk. 
A file watcher.

<img src="https://raw.github.com/schell/steeloverseer/master/rsrc/pic.jpg" width="300" title="steel overseer" />

installation
------------
After cloning and cd'ing into the repo run ```cabal install```, which should install the ```sos``` executable in your $HOME/.cabal/bin.

usage
-----
    sos: Usage: sos [v] -c command -e file_extension
      -v                --version                  show version info
      -c ADD_COMMAND    --command=ADD_COMMAND      add command to run on file event
      -e ADD_EXTENSION  --extension=ADD_EXTENSION  add file extension to watch for events

<img src="screen.png" title="steel overseer screenshot" />

