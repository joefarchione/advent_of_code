open Core
open Advent2025_lib.Day03

let %expect_test "part1_example" = 
  solve1 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day03_example.txt";
  [%expect {| 357 |}]
;;

let %expect_test "part1" = 
  solve1 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day03.txt";
  [%expect {| 17324 |}]
;;

let %expect_test "part2_example" = 
  solve2 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day03_example.txt";
  [%expect {| 3121910778619 |}]
;;

let %expect_test "part2" = 
  solve2 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day03.txt";
  [%expect {| 171846613143331 |}]
;;