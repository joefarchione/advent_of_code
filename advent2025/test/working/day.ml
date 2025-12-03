open! Core
open! Advent2025_lib

let get_input_path ?(example=false) day = 
  let input_dir = "/home/joefarchione/Projects/advent_of_code/advent2025/inputs" in
  let filename = if example then (Printf.sprintf "day%02d_example.txt" day) else (Printf.sprintf "day%02d.txt" day) in 
  Filename.concat input_dir filename


let%expect_test "solutions" =
  printf "%d\n" (Day03.solve1 (get_input_path ~example:true 3));
  printf "%d\n" (Day03.solve1 (get_input_path ~example:false 3));
  printf "%d\n" (Day03.solve2 (get_input_path ~example:true 3));
  printf "%d\n" (Day03.solve2 (get_input_path ~example:false 3));
  [%expect {|
    357
    17324
    3121910778619
    171846613143331
    |}]
;;


