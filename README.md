Steel Overseer
==============

A file watcher and development tool, similar to Ruby's [Guard](https://github.com/guard/guard).

[![Build Status](https://travis-ci.org/steeloverseer/steeloverseer.png?branch=master)](https://travis-ci.org/steeloverseer/steeloverseer)

Installation
============

Download and install the [stack](https://github.com/commercialhaskell/stack) build tool.

    stack install steeloverseer

This will create a binary deep inside `~/.stack/`, and symlink to it at
`~/.local/bin/sos`.

Usage
=====

See `sos --help` to get started:

    Steel Overseer 2.0.1

    Usage: sos [TARGET] [--rcfile ARG] [-c|--command COMMAND] [-p|--pattern PATTERN]
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

Patterns and Commands
-------------------
Capture groups can be created with `(` `)` and captured variables can be
referred to with `\1`, `\2`, etc. (`\0` contains the entire match).

For example, for each change to a `.c` file in `src/`, we may want to compile
the file and run its corresponding unit test:

    sos src/ -c "gcc -c \0 -o obj/\1.o" -c "make test --filter=test/\1_test.c" -p "src/(.*)\.c"

Commands are run left-to-right, and one failed command will halt the entire pipeline.

The RCFile
----------
As a shortcut, we may want to write the above only once and save it in `.sosrc`,
which is an alternative to the command-line interface (yaml syntax):

```yaml
- pattern: src/(.*)\.c
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
    entry            := { "pattern" | "patterns" : value | [value]
                        , "command" | "commands" : value | [value]
                        }
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
