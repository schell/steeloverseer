Steel Overseer
==============
> <img src="https://raw.github.com/schell/steeloverseer/master/rsrc/pic.jpg" width="300" title="flavor text" />
>
> The world is already run by all manner of machines. One day, they'll remind us of that fact. 
> 
> -Sargis Haz, artificer 

It is
-----
A tool that runs a list of commands after files change on disk. 
A file watcher.

Installation
------------
After cloning and cd'ing into the repo run ```cabal install```, which should install the ```sos``` executable in your $HOME/.cabal/bin.

Usage
-----
    sos: Usage: sos [v] -c command -e file_extension
      -v                --version                  show version info
      -c ADD_COMMAND    --command=ADD_COMMAND      add command to run on file event
      -e ADD_EXTENSION  --extension=ADD_EXTENSION  add file extension to watch for events

<img src="https://raw.github.com/schell/steeloverseer/master/rsrc/screen.png" title="steel overseer screenshot" />

[Art above by Chris Rahn for Wizards of the Coast](http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=205036 "Steel Overseer")
