Steel Overseer
==============
> <img src="https://raw.github.com/schell/steeloverseer/master/rsrc/pic.jpg" width="300" title="flavor text" />
>
> The world is already run by all manner of machines. One day, they'll remind us of that fact. 
> 
> -Sargis Haz, artificer 

It is
-----
A command line tool that responds to filesystem events. 
Users can provide regular expressions to match on filepaths 
and shell commands to execute in serial when matches are found. 
Displays text using a subset of available primary colors.

Specifically
------------
A filesystem event occurs when a file is added, deleted or updated. 
If this event happens in the directory you specified using 
```-d DIRECTORY```, steeloverseer will match the event's filepath 
against the patterns you provided with ```-p PATTERN```.
If a match is found steeloverseer will run the commands provided 
with ```-c COMMAND``` in serial until all exit successfully, one fails or one hangs.

You can provide multiple patterns and multiple commands, ie:

    sos -c "git status" -c "echo hi world" -p "hs" -p "md"
    
This will execute ```git status``` followed by ```echo hi world``` 
whenever files matching "hs" or "md" are changed. ```-d DIRECTORY``` 
is not provided above, so it's assumed to be ```./```.

Also, since ```-p PATTERN``` are regular expressions we can do the same as above with:

    sos -c "git status" -c "echo hi world" -p "hs|md"
    
Of course this would run whenever any match on "hs|md" is found, 
for instance on the filepath ```/Users/home/mdman/file.txt```.
For extensions it may make sense to use the endline matcher:

    sos -c "git status" -c "echo hi world" -p "hs$|md$"

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
