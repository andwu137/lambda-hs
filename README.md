# lambda-hs

Sorry for not using github features for features/issues :)

## How To Run:
- Load File with Interpreter: `stack run -- -i filename`, e.g. `stack run -- -i input/test.lb`
- Run Interpreter: `stack run` or `stack run -- -i`

[![asciinema: a repl example of lists](https://asciinema.org/a/QJQu6DIYp2PetuRg2XDXhtBAz.svg)](https://asciinema.org/a/QJQu6DIYp2PetuRg2XDXhtBAz?autoplay=1)

## TODO:
- add a test suite for the parser and evaluation
- repl/cli
    - add multilining
    - special commands while in the interpreter (:q, :r)
    - add more commandline options
- evaluation:
    - strictness
- improve error messages:
    - Store line counts for symbols
- operators:
    - Add precedence parsing
    - Allow cool syntax for definitions of infix functions

## Issues:
- letexpr and expr are called during assignment parsing (performance)
