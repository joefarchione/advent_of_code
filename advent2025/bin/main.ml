open Core
open Advent2025_lib

let solve (day : int) (solve1 : string -> int) (solve2 : string -> int) =
  (*
    Assumes folder ~/aoc contains /.aoc_cookie with session cookie for adventofcode.com
  *)
  let input = Advent2025_utils.Input.get 2025 day in
  let result1 = solve1 input in
  let result2 = solve2 input in
  printf "Day%02d: (%d, %d)\n" day result1 result2

let () =
  solve 1 Day01.solve1 Day01.solve2;
  solve 2 Day02.solve1 Day02.solve2;
  solve 3 Day03.solve1 Day03.solve2;
  solve 4 Day04.solve1 Day04.solve2;
  solve 5 Day05.solve1 Day05.solve2;
  solve 6 Day06.solve1 Day06.solve2;
  solve 7 Day07.solve1 Day07.solve2;
  solve 8 Day08.solve1 Day08.solve2;
  solve 9 Day09.solve1 Day09.solve2;
  solve 10 Day10.solve1 Day10.solve2;
  solve 11 Day11.solve1 Day11.solve2;
  solve 12 Day12.solve1 (fun _ -> 0)
