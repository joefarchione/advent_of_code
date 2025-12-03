open Core
open Advent2025_lib

let solve day_str solve1 solve2 = 
  let input_dir = "/home/joefarchione/Projects/advent_of_code/advent2025/inputs" in
  let filepath = Filename.concat input_dir (Printf.sprintf "day%s.txt" day_str) in
  printf "Day%s: (" day_str;
  solve1 filepath;
  printf ", ";
  solve2 filepath;
  printf ")\n"


let () =
  solve "01" Day01.solve1 Day01.solve2;
  solve "02" Day02.solve1 Day02.solve2;
  solve "03" Day03.solve1 Day03.solve2;
