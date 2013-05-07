steel overseer
==============
A tool that runs a command after files change on disk. 
A file watcher.

<img src="https://raw.github.com/schell/steeloverseer/master/rsrc/pic.jpg" width="300" title="steel overseer" border="2"/>

installation
------------
After cloning and cd'ing into the repo run ```cabal install```, which should install the ```sos``` executable in your $HOME/.cabal/bin.

usage
-----

    sos: usage: sos [vV] [iI] cC [file extensions...]
      -v, -V                  --version          show version number
      -i, -I                  --init             run command at startup
      -c command, -C command  --command=command  command to run on change

