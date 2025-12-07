open Aoc

let read input =
  let dimensions =
    ( String.split_lines input |> List.length,
      List.hd_exn (String.split_lines input) |> String.length )
  in
  String.split_lines input |> List.map ~f:String.to_list
  |> List.mapi ~f:(fun row line ->
         List.filter_mapi line ~f:(fun col ch ->
             if Char.equal ch '.' then None else Some (row, col)))
  |> List.concat |> List.hd_tl_exn
  |> fun (start, splitters) ->
  (start, Coord2Set.of_list splitters, Grid.create dimensions)

let split (n, m) splitter =
  let left =
    Coord2d.left splitter |> fun v -> Option.some_if (Coord2d.in_grid n m v) v
  in
  let right =
    Coord2d.right splitter |> fun v -> Option.some_if (Coord2d.in_grid n m v) v
  in
  (left, right)

let next grid beam =
  let next_beam = Coord2d.down beam in
  Option.some_if (Grid.contains grid next_beam) next_beam

let count_number_of_splits grid start splitters =
  let split = split grid in
  let next = next grid in
  let rec aux beam count visited =
    match beam with
    | None -> (count, visited)
    | Some b ->
        if Set.mem visited b then (0, visited)
        else if Set.mem splitters b then
          let left, right = split b in
          let count_left, visited = aux left 0 visited in
          let count_right, visited = aux right 0 visited in
          (1 + count_left + count_right, Set.add visited b)
        else aux (next b) count (Set.add visited b)
  in
  aux (Some start) 0 Coord2Set.empty |> fst

let count_unique_paths grid start splitters =
  let split = split grid in
  let next = next grid in
  let cache = Hashtbl.Poly.create () in
  let rec aux beam count =
    match Hashtbl.find cache beam with
    | Some v -> v
    | None -> (
        match beam with
        | None -> count
        | Some b ->
            let new_count =
              if Set.mem splitters b then
                let left, right = split b in
                let count_left, count_right = (aux left 1, aux right 1) in
                count_left + count_right
              else aux (next b) count
            in
            Hashtbl.set cache ~key:beam ~data:new_count;
            new_count)
  in
  aux (Some start) 1

let solve1 input =
  let start, splitters, grid = read input in
  count_number_of_splits grid start splitters

let solve2 input =
  let start, splitters, grid = read input in
  count_unique_paths grid start splitters
