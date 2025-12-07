open Aoc

let read =
  String.split_lines
  >> List.mapi ~f:(fun i line ->
         line |> String.to_list
         |> List.mapi ~f:(fun j v -> ((i, j), v))
         |> List.filter ~f:(fun (_, v) -> Char.equal v '@')
         |> List.map ~f:fst)
  >> List.concat >> Pair.Coord2Set.of_list

let adjacent_coords (x, y) = Pair.Coord2d.neighbors (x, y)

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
    match towels |> Set.filter ~f:(fewer_than_four_adjacent towels) with
    | remove when Set.is_empty remove -> count
    | remove -> aux (Set.diff towels remove) (count + Set.length remove)
  in
  aux (read filepath) 0
