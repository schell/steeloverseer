Steel Overseer
==============
Forked from **https://github.com/schell/steeloverseer**

Changes from the original version:

- Support for checking a single file, e.g. `sos foo.hs -c "ghc foo.hs"`
- *Very* rudimentary automatic command inference based on file type, e.g. `sos foo.hs` will run "stack ghc foo.hs"
- Somewhat simpler internals with no hard-coded 1s wait to batch commands (things just run right away)
- *.sosrc* support, which is a YAML file that contains entries of the form:

```yaml    
- pattern: .*.txt
  command: echo txt file changed
- pattern: .*.php
  command: echo php file changed
```
(these entries are simply combined with the CLI-specified commands via `-p` and `-c`)
