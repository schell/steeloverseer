Steel Overseer
==============
Forked from **https://github.com/schell/steeloverseer**

Changes from the original version:

- Support for checking a single file, e.g. `sos foo.hs`
- Capture groups in regex patterns, e.g. `sos . -c "gcc -c {0}" -p "(.*\.c)"`
- Somewhat simplified internals with no hard-coded 1s wait to batch commands (things just run right away)
- *.sosrc* support, which is a yaml file that contains entries such as:

```yaml
- pattern: src/(.*)\.c
  commands:
  - make
  - make test --file=test/{1}_test.c
- pattern: .*\.hs
  commands:
  - hlint {0}
```
(these entries are simply combined with the CLI-specified commands via `-p` and `-c`)
