open! Core

module IntPair = struct
  type t = int * int [@@deriving compare, sexp, hash]
end

module IntPairSet = Set.Make (IntPair)

let adjacent_coords (x, y) =
  [
    (x - 1, y + 1);
    (x - 1, y);
    (x - 1, y - 1);
    (x + 1, y + 1);
    (x + 1, y);
    (x + 1, y - 1);
    (x, y + 1);
    (x, y - 1);
  ]

let read filepath =
  In_channel.read_lines filepath
  |> List.mapi ~f:(fun i line ->
         line |> String.to_list
         |> List.mapi ~f:(fun j v -> ((i, j), v))
         |> List.filter ~f:(fun (_, v) -> Char.equal v '@')
         |> List.map ~f:(fun (coord, _) -> coord))
  |> List.concat |> IntPairSet.of_list

let fewer_than_four_adjacent grid coord =
  adjacent_coords coord
  |> List.filter ~f:(fun c -> Set.mem grid c)
  |> List.length
  |> fun c -> c < 4

let solve1 filepath =
  let grid = read filepath in
  grid |> Set.filter ~f:(fewer_than_four_adjacent grid) |> Set.length

let solve2 filepath =
  let rec aux towels count =
    if towels |> Set.filter ~f:(fewer_than_four_adjacent towels) |> Set.is_empty
    then count
    else
      let remove = towels |> Set.filter ~f:(fewer_than_four_adjacent towels) in
      let rem_towels = Set.diff towels remove in
      aux rem_towels (count + Set.length remove)
  in
  aux (read filepath) 0
