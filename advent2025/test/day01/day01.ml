open Core
open Advent2025_lib.Day01

let %expect_test "part1_example" = 
  solve1 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day01_example.txt";
  [%expect {| 3 |}]

let %expect_test "part1" = 
  solve1 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day01.txt";
  [%expect {| 1026 |}]

let %expect_test "part2_example" = 
  solve2 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day01_example.txt";
  [%expect {| 6 |}]

let %expect_test "part2" = 
  solve2 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day01.txt";
  [%expect {| 5923 |}]