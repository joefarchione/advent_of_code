open Aoc

let read input =
  input |> String.strip |> Input.lines |> List.map ~f:Coord2.of_string

let calc_area ((x1, y1), (x2, y2)) =
  (Int.abs (x1 - x2) + 1) * (Int.abs (y1 - y2) + 1)

let solve1 input =
  read input |> List.pairs |> List.map ~f:calc_area
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

let solve2 input =
  let red_tiles = read input in
  let edges =
    (* connect all red tiles - they WRAP around *)
    List.zip_exn red_tiles (List.tl_exn red_tiles @ [ List.hd_exn red_tiles ])
  in
  let edges_v =
    List.filter edges ~f:(fun ((x1, _), (x2, _)) -> Int.equal x1 x2)
    |> List.map ~f:(fun ((x1, y1), (_, y2)) ->
           (x1, Int.min y1 y2, Int.max y1 y2))
  in
  let edges_h =
    List.filter edges ~f:(fun ((_, y1), (_, y2)) -> Int.equal y1 y2)
    |> List.map ~f:(fun ((x1, y1), (x2, _)) ->
           (y1, Int.min x1 x2, Int.max x1 x2))
  in

  List.pairs red_tiles
  |> List.fold ~init:0 ~f:(fun acc ((x1, y1), (x2, y2)) ->
         let min_x, max_x = Tuple2.min_max (x1, x2) in
         let min_y, max_y = Tuple2.min_max (y1, y2) in
         let has_edge_crossing =
           List.exists edges_v ~f:(fun (ex, ey1, ey2) ->
               ex > min_x && ex < max_x && not (ey1 >= max_y || ey2 <= min_y))
           || List.exists edges_h ~f:(fun (ey, ex1, ex2) ->
                  ey > min_y && ey < max_y && not (ex1 >= max_x || ex2 <= min_x))
         in
         if has_edge_crossing then acc
         else Int.max acc (calc_area ((min_x, min_y), (max_x, max_y))))
