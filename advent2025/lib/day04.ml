open! Core

let read filepath =
  In_channel.read_lines filepath
  |> List.mapi ~f:(fun i line ->
         line |> String.to_list
         |> List.mapi ~f:(fun j v -> ((i, j), v))
         |> List.filter ~f:(fun (_, v) -> Char.equal v '@')
         |> List.map ~f:fst)
  |> List.concat |> Types.IntPairSet.of_list

let adjacent_coords (x, y) = Types.IntPair.neighbors (x, y)

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
