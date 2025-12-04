open! Core
open! Advent2025_lib

let solve (day : int) (solve1 : string -> int) (solve2 : string -> int) =
  let input_dir =
    "/home/joefarchione/Projects/advent_of_code/advent2025/inputs"
  in
  let filepath = Filename.concat input_dir (Printf.sprintf "day%02d.txt" day) in
  print_endline
    ((sprintf "Day%02d: (%d, %d)") day (solve1 filepath) (solve2 filepath))

let%expect_test "calendar" =
  solve 1 Day01.solve1 Day01.solve2;
  solve 2 Day02.solve1 Day02.solve2;
  solve 3 Day03.solve1 Day03.solve2;
  solve 4 Day04.solve1 Day04.solve2;
  [%expect
    {|
    Day01: (1026, 5923)
    Day02: (56660955519, 79183223243)
    Day03: (17324, 171846613143331)
    Day04: (1464, 8409)
    |}]
