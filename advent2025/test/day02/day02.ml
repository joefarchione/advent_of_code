open Core
open Advent2025_lib.Day02

let %expect_test "part1_example" = 
  solve1 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day02_example.txt";
  [%expect {| 1227775554 |}]
;;


let %expect_test "part1" = 
  solve1 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day02.txt";
  [%expect {| 56660955519 |}]
;;


let %expect_test "part2_example" = 
  solve2 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day02_example.txt";
  [%expect {| 4174379265 |}]
;;

let %expect_test "part2" = 
  solve2 "/home/joefarchione/Projects/advent_of_code/advent2025/inputs/day02.txt";
  [%expect {| 79183223243 |}]
;;

