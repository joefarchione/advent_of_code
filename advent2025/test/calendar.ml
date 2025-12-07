open! Core
open! Advent2025_lib

let solve (day : int) (solve1 : string -> int) (solve2 : string -> int) =
  let input = Advent2025_utils.Io.get_aoc_input_my_cookie 2025 day in
  let result1 = solve1 input in
  let result2 = solve2 input in
  printf "Day%02d: (%d, %d)\n" day result1 result2

let%expect_test "calendar" =
  solve 1 Day01.solve1 Day01.solve2;
  solve 2 Day02.solve1 Day02.solve2;
  solve 3 Day03.solve1 Day03.solve2;
  solve 4 Day04.solve1 Day04.solve2;
  solve 5 Day05.solve1 Day05.solve2;
  solve 6 Day06.solve1 Day06.solve2;
  solve 7 Day07.solve1 Day07.solve2;
  [%expect
    {|
    Day01: (1026, 5923)
    Day02: (56660955519, 79183223243)
    Day03: (17324, 171846613143331)
    Day04: (1464, 8409)
    Day05: (698, 352807801032167)
    Day06: (4648618073226, 7329921182115)
    Day07: (1555, 12895232295789)
    |}]
