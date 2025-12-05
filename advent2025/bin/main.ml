open Core
open Advent2025_lib

let solve day solve1 solve2 =
  let input_dir =
    "/home/joefarchione/Projects/advent_of_code/advent2025/inputs"
  in
  let filepath = Filename.concat input_dir (Printf.sprintf "day%02d.txt" day) in
  print_endline
    ((sprintf "Day%d: \n\tpart 1: %d\n\tpart 2: %d\n")
       day (solve1 filepath) (solve2 filepath))

let () =
  solve 1 Day01.solve1 Day01.solve2;
  solve 2 Day02.solve1 Day02.solve2;
  solve 3 Day03.solve1 Day03.solve2;
  solve 4 Day04.solve1 Day04.solve2;
  solve 5 Day05.solve1 Day05.solve2
