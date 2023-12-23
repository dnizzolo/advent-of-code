(asdf:defsystem :advent-of-code
  :description "Advent of Code"
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :depends-on (:parachute
               :alexandria
               :serapeum
               :one-more-re-nightmare
               :damn-fast-priority-queue
               :queue
               :magicl)
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
                             (:file "day19")))
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
                             (:file "day18"))))
  :in-order-to ((asdf:test-op (asdf:test-op :advent-of-code/test))))

(asdf:defsystem :advent-of-code/test
  :description "Test suite for Advent of Code"
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :depends-on (:advent-of-code)
  :perform (test-op (op c)
                    (uiop:symbol-call :parachute :test
                                      (uiop:symbol-call :parachute :test-packages))))
