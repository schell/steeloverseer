Steel Overseer
==============

A file watcher and development tool, similar to Ruby's [Guard](https://github.com/guard/guard).

The main idea is that you have steeloverseer watch your files and then execute a series of shell 
commands in response. The first command to fail short circuits the series. The watched files can 
be selected using regular expressions and the commands may include capture groups.

[![Build Status](https://travis-ci.org/steeloverseer/steeloverseer.png?branch=master)](https://travis-ci.org/steeloverseer/steeloverseer)
[![Build status](https://github.com/schell/steeloverseer/actions/workflows/stack.yml/badge.svg)](https://github.com/schell/steeloverseer/actions/workflows/stack.yml)
[![Build status](https://github.com/schell/steeloverseer/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/schell/steeloverseer/actions/workflows/haskell-ci.yml)


Installation
============

Download and install the [stack](https://github.com/commercialhaskell/stack) build tool.

    stack install steeloverseer

This will create a binary deep inside `~/.stack/`, and symlink to it at
`~/.local/bin/sos`.

Usage
=====

See `sos --help` to get started:

    Steel Overseer 2.0.2

    Usage: sos [TARGET] [--rcfile ARG] [-c|--command COMMAND] [-p|--pattern PATTERN]
               [-e|--exclude PATTERN]
      A file watcher and development tool.

    Available options:
      -h,--help                Show this help text
      TARGET                   Optional file or directory to watch for
                               changes. (default: ".")
      --rcfile ARG             Optional rcfile to read patterns and commands
                               from. (default: ".sosrc")
      -c,--command COMMAND     Add command to run on file event.
      -p,--pattern PATTERN     Add pattern to match on file path. Only relevant if
                               the target is a directory. (default: .*)
      -e,--exclude PATTERN     Add pattern to exclude matches on file path. Only
                               relevant if the target is a directory.


Patterns and Commands
-------------------
Capture groups can be created with `(` `)` and captured variables can be
referred to with `\1`, `\2`, etc. (`\0` contains the entire match).

For example, for each change to a `.c` file in `src/` (excluding files
containing `"_test"`), we may want to compile the file and run its corresponding
unit test:

    sos src/ -c "gcc -c \0 -o obj/\1.o" -c "make test --filter=test/\1_test.c" -p "src/(.*)\.c" -e "_test"

Commands are run left-to-right, and one failed command will halt the entire pipeline.

The RCFile
----------
As a shortcut, we may want to write the above only once and save it in `.sosrc`,
which is an alternative to the command-line interface (yaml syntax):

```yaml
- pattern: src/(.*)\.c
  exclude: _test
  commands:
  - gcc -c \0 -o obj/\1.o
  - make test --filter=test/\1_test.c
```

Then, we only need to run:

    sos

to start watching the current directory. If you'd like to use multiple rcfiles,
or just don't like the name `.sosrc` you can specify the rcfile on the command
line like so:

    sos --rcfile my-rcfile

### Grammar

    sosrc            := [entry]
    entry            := {
                          pattern_entry,
                          exclude_entry?, -- Note: optional!
                          command_entry
                        }
    pattern_entry    := "pattern" | "patterns" : value | [value]
    exclude_entry    := "exclude" | "excludes" | "excluding" : value | [value]
    command_entry    := "command" | "commands" : value | [value]
    value            := [segment]
    segment          := text_segment | var_segment
    text_segment     := string
    var_segment      := '\' integer

The .sosrc grammar is somewhat flexible with respect to the command
specifications. Both singular and plural keys are allowed, and both strings
and lists of strings are allowed for values.

Pipelining Explaned
-------------------
Pipelines of commands are immediately canceled and re-run if a subsequent
filesystem event triggers the *same* list of commands. Otherwise, commands are
are enqueued and run sequentially to keep the terminal output clean and readable.

For example, we may wish to run `hlint` on any modified `.hs` file:

```yaml
- pattern: .*\.hs
  command: hlint \0
```

We can modify `foo.hs` and trigger `hlint foo.hs` to run. During its execution,
modifying `bar.hs` will *enqueue* `hlint bar.hs`, while modifying `foo.hs` again
will *re-run* `hlint foo.hs`.

Transient Files
---------------
Sometimes text editors and other programs create short lived files in the 
directories that `sos` is watching. These can trigger `sos` to run your 
pipeline. This can often be avoided by using precise include syntax, ie 
adding explicit matchers like an end-line match:

```
- pattern: .*\.tex$ 
```

Alternatively you may use exclude syntax to exclude any transient editor files 
(eg here's an sosrc used for editing Haskell doctests and ignoring emac's flycheck files):

```
# This is for testing documentation
- patterns:
  - .*/[^_]*\.l?hs$
  excludes:
  - \#
  - flycheck
  commands:
  - stack exec doctest -- \0
```

For more info, see https://github.com/schell/steeloverseer/issues/38
