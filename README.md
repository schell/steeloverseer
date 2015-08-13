Steel Overseer
==============
> ![flavor text](https://raw.github.com/schell/steeloverseer/master/rsrc/pic.jpg)
>
> The world is already run by all manner of machines. One day, they'll remind us of that fact. 
> 
> -Sargis Haz, artificer 

[![Build Status](https://travis-ci.org/schell/steeloverseer.png?branch=master)](https://travis-ci.org/schell/steeloverseer)

It is
-----
A development tool that runs commands when certain files are updated, added or
deleted.

Steeloverseer watches files whose names match a regular expression and then 
runs a series of commands when those files are updated. 

Specifically
------------
A filesystem event occurs when a file is added, deleted or updated. 
If this event happens on a file that matches one of the patterns provided with 
the `-p PATTERN` flag then steeloverseer will run the commands provided 
with the `-c COMMAND` flag. These commands will be performed in serial until 
one hangs, fails or all exit successfully.

You can provide multiple patterns and multiple commands, ie:

    sos -c "git status" -c "echo hi world" -p "hs" -p "md"

You can seperately specify the directory to run in with `-d DIRECTORY`. The 
default is `.`.
    
This will execute `git status` followed by `echo hi world` 
whenever files matching "hs" or "md" are changed. `-d DIRECTORY` 
is not provided above, so it's assumed to be `./`.

Also, since `-p PATTERN` are regular expressions we can do the same as above with:

    sos -c "git status" -c "echo hi world" -p "hs|md"
    
Of course this would run whenever any match on "hs|md" is found, 
for instance on the filepath `/Users/home/mdman/file.txt`.
For extensions it may make sense to use the endline matcher:

    sos -c "git status" -c "echo hi world" -p "hs$|md$"

Installation
------------
Using cabal, `cabal install steeloverseer`.

Usage
-----
    sos: Usage: sos [v] -c command -p pattern
      -v            --version              show version info
      -c COMMAND    --command=COMMAND      add command to run on file event
      -p PATTERN    --pattern=PATTERN      add pattern to match on file path
      -d DIRECTORY  --directory=DIRECTORY  set directory to watch for changes (default is ./)

![steeloverseer screencast](http://zyghost.com/images/sos.gif)

Future
------
Project `.sosrc` file for specifying multiple sos commands while working on a project (@see issue #4)

[Art above by Chris Rahn for Wizards of the Coast](http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=205036 "Steel Overseer")
