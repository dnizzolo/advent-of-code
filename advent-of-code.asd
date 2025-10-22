(defsystem "advent-of-code"
  :description "Advent of Code"
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :maintainer "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :homepage "https://github.com/dnizzolo/advent-of-code"
  :source-control (:git "https://github.com/dnizzolo/advent-of-code.git")
  :bug-tracker "https://github.com/dnizzolo/advent-of-code/issues"
  :depends-on ("parachute"
               "alexandria"
               "serapeum"
               "cl-ppcre"
               "min-priority-queue"
               "queue")
  :serial t
  :components ((:file "utils")
               (:module "2021"
                :components ((:file "day01")
                             (:file "day02")
                             (:file "day03")
                             (:file "day04")
                             (:file "day05")
                             (:file "day06")
                             (:file "day07")
                             (:file "day08")
                             (:file "day09")
                             (:file "day10")
                             (:file "day11")
                             (:file "day12")
                             (:file "day13")
                             (:file "day14")
                             (:file "day15")
                             (:file "day16")
                             (:file "day17")
                             (:file "day18")
                             (:file "day19")
                             (:file "day20")))
               (:module "2022"
                :components ((:file "day01")
                             (:file "day02")
                             (:file "day03")
                             (:file "day04")
                             (:file "day05")
                             (:file "day06")
                             (:file "day07")
                             (:file "day08")
                             (:file "day09")
                             (:file "day10")
                             (:file "day11")
                             (:file "day12")
                             (:file "day13")
                             (:file "day14")
                             (:file "day15")))
               (:module "2023"
                :components ((:file "day01")
                             (:file "day02")
                             (:file "day03")
                             (:file "day04")
                             (:file "day05")
                             (:file "day06")
                             (:file "day07")
                             (:file "day08")
                             (:file "day09")
                             (:file "day10")
                             (:file "day11")
                             (:file "day12")
                             (:file "day13")
                             (:file "day14")
                             (:file "day15")
                             (:file "day16")
                             (:file "day17")
                             (:file "day18")
                             (:file "day19")
                             (:file "day20")
                             (:file "day21")
                             (:file "day25")))
               (:module "2024"
                :components ((:file "day01")
                             (:file "day02")
                             (:file "day03")
                             (:file "day04")
                             (:file "day05")
                             (:file "day06")
                             (:file "day07")
                             (:file "day08")
                             (:file "day09")
                             (:file "day10")
                             (:file "day11")
                             (:file "day13")
                             (:file "day14")
                             (:file "day19")
                             (:file "day22"))))
  :in-order-to ((asdf:test-op (asdf:test-op "advent-of-code/test"))))

(defsystem "advent-of-code/test"
  :description "Test suite for Advent of Code"
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :maintainer "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :homepage "https://github.com/dnizzolo/advent-of-code"
  :source-control (:git "https://github.com/dnizzolo/advent-of-code.git")
  :bug-tracker "https://github.com/dnizzolo/advent-of-code/issues"
  :depends-on ("advent-of-code")
  :perform (test-op (op c) (symbol-call
                            :parachute
                            :test
                            (symbol-call :parachute :test-packages))))
