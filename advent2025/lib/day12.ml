open Aoc

let parse_shape s =
  String.to_list s |> List.filter ~f:(Char.equal '#') |> List.length

let parse_grid s =
  let digits =
    Re.all Re.(rep1 digit |> compile) s
    |> List.map ~f:(fun g -> Re.Group.get g 0 |> Int.of_string)
  in
  match digits with
  | x :: y :: tl -> ((x, y), tl)
  | _ -> failwith "Invalid grid input"

let read input =
  match input |> String.split_on_empty_line |> List.rev with
  | g :: tl ->
      let shapes = tl |> List.rev |> List.map ~f:parse_shape in
      let grids = g |> String.split_lines |> List.map ~f:parse_grid in
      (shapes, grids)
  | _ -> failwith "Invalid input"

let can_fit shapes ((n, m), num_shapes) =
  List.zip_exn num_shapes shapes
  |> List.map ~f:(fun (num, shape) -> num * shape)
  |> List.fold1 ( + )
  |> fun shape_area -> n * m >= shape_area

let solve1 input =
  let shapes, grids = read input in
  grids |> List.filter ~f:(can_fit shapes) |> List.length
