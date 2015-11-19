Steel Overseer
==============

A file watcher and development tool, similar to Ruby's [Guard](https://github.com/guard/guard).

Forked and extended from **https://github.com/schell/steeloverseer**

Usage
=====

`sos` requires a directory to watch, a list of commands to run, and an optional
list of regex patterns to match on file paths. Capture groups can be created
with `(` `)` and captured variables can be referred to with `{1}`, `{2}`, etc.
(`{0}` contains the entire match).

For example, for each `.c` file in `src/`, we may want to:

- Compile the file
- Run its corresponding unit test

    sos src/ -c "gcc -c {0} -o obj/{1}.o" -c "make test --filter=test/{1}_test.c" -p "src/(.*)\.c"

Commands are run left-to-right, and one failed command will halt the entire pipeline.

As a shortcut, we may want to write the above only once and save it in `.sosrc`, which is
an alternative to the command-line interface (yaml syntax):

```yaml
- pattern: src/(.*)\.c
  commands:
  - gcc -c {0} -o obj/{1}.o
  - make test --filter=test/{1}_test.c
```

Pipelines of commands are immediately canceled and re-run if a subsequent
filesystem event triggers the *same* list of commands. Otherwise, commands are
are enqueued and run sequentially to keep the terminal output clean and readable.

For example, we may wish to run `hlint` on any modified `.hs` file:

```yaml
- pattern: .*\.hs
  commands:
  - hlint {0}
```

We can modify `foo.hs` and trigger `hlint foo.hs` to run. During its execution,
modifying `bar.hs` will *enqueue* `hlint bar.hs`, while modifying `foo.hs` again
will *re-run* `hlint foo.hs`.
