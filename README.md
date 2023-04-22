# Advent of Code

My solutions to [Advent of Code](https://adventofcode.com/).

## Structure

Each day of each year is implemented in a package of the form
`:aoc<year>.<day>` which has a function named `day<day>`.

## Testing

To run all tests use `(asdf:test-system :advent-of-code)`, e.g.:

```
CL-USER> (asdf:test-system :advent-of-code)
```

In order to run a test for a specific day call `parachute:test` with
the day's package, e.g. for day 14 or the 2021 event:

```
AOC2021.14> (parachute:test *package*)
```
