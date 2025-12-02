open Core
open Advent2025_lib

let () =
  let advent (input_dir: string) = 
    printf "Day 01 - part 1: "; (Day01.solve1 (Filename.concat input_dir "day01.txt")); printf "\n";
    printf "Day 01 - part 2: "; (Day01.solve2 (Filename.concat input_dir "day01.txt")); printf "\n";
    printf "Day 02 - part 1: "; (Day02.solve1 (Filename.concat input_dir "day02.txt")); printf "\n";
    printf "Day 02 - part 2: "; (Day02.solve2 (Filename.concat input_dir "day02.txt")); printf "\n";
  in
  advent "/home/joefarchione/Projects/advent_of_code/advent2025/inputs"
