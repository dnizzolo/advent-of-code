# Advent of Code

My solutions to [Advent of Code](https://adventofcode.com/).

## Structure

Each day of each year is implemented in a package of the form
`:aoc<year>.<day>` which has a function named `day<day>` that returns
the solutions of that day's part 1 and part 2 as multiple values. For
example: day 11 from year 2021 is implemented in the package
`:aoc2021.11` and has a function `day11`.

## Testing

To run all tests use `(asdf:test-system :advent-of-code)`:

```
CL-USER> (asdf:test-system :advent-of-code)
```

In order to run a test for a specific day call `parachute:test` with
that day's package as argument, e.g. for day 14 of the 2021 event or
day 10 of the 2022 event:

```
AOC2021.14> (parachute:test *package*)
...
AOC2021.14> (parachute:test (find-package :aoc2022.10))
...
```

## Pitfalls

This repository doesn't include the problems' inputs as per this
[rule](https://www.reddit.com/r/adventofcode/wiki/troubleshooting/no_asking_for_inputs/),
so you can't just clone it and run the tests or the code.
