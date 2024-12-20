# Advent of Code

My solutions to [Advent of Code](https://adventofcode.com/).

## Structure

Each solution is implemented in a package named `:aoc<year>.<day>` as
a function whose name is `day<day>` that returns the solutions of that
day's part 1 and part 2 as multiple values. For example: day 11 of
year 2021 is implemented in the package `:aoc2021.11` as the function
`day11`.

## Testing

To run all tests use `(asdf:test-system :advent-of-code)` from the
Lisp REPL. In order to run a test for a specific day call
`parachute:test` with that day's package as argument, for example for
day 14 of the 2021 event or day 10 of the 2022 event:

```
AOC2021.14> (parachute:test *package*)
; Omitted test output.
AOC2021.14> (parachute:test (find-package :aoc2022.10))
; Omitted test output.
```

## Pitfalls

This repository doesn't include the problems' inputs or examples, see
[this](https://adventofcode.com/about). You can't just clone it and
run the tests or the code right away, you'll need to provide the
inputs yourself which will likely be different from my own so the
tests as they are defined probably don't make any sense for you. Also,
I am using some local libraries I have written for simple data
structures such as queues and priority queues which I haven't included
in this system.
