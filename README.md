Steel Overseer
==============
> <img src="https://raw.github.com/schell/steeloverseer/master/rsrc/pic.jpg" width="300" title="flavor text" />
>
> The world is already run by all manner of machines. One day, they'll remind us of that fact. 
> 
> -Sargis Haz, artificer 

It is
-----
A command line tool that responds to filesystem events. Users can provide regular expressions to match on filepaths and shell commands to execute in serial when matches are found. Displays text using a subset of available primary colors.

Installation
------------
Using cabal, ```cabal install steeloverseer```.

Usage
-----
    sos: Usage: sos [v] -c command -p pattern
      -v            --version              show version info
      -c COMMAND    --command=COMMAND      add command to run on file event
      -p PATTERN    --pattern=PATTERN      add pattern to match on file path
      -d DIRECTORY  --directory=DIRECTORY  set directory to watch for changes (default is ./)

<img src="https://raw.github.com/schell/steeloverseer/master/rsrc/screenv0.2.0.0.png" title="steel overseer screenshot" />

[Art above by Chris Rahn for Wizards of the Coast](http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=205036 "Steel Overseer")
