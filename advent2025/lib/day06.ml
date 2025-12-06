open Aoc

let split_line = Str.split (Str.regexp "[ \t]+")

let parse_input_p1 filepath =
  In_channel.read_lines filepath |> List.map ~f:split_line |> List.zip_n

let parse_input_p2 filepath =
  In_channel.read_lines filepath |> List.rev |> List.hd_tl_exn |> fun (a, b) ->
  (a, List.rev b) |> fun (ops, numbers) ->
  ( split_line ops,
    numbers |> List.map ~f:String.to_list |> List.transpose_exn
    |> List.map ~f:String.of_list
    |> List.split (String.for_all ~f:Char.is_whitespace)
    |> List.rev )
  |> fun (ops, numbers) ->
  List.zip_exn numbers ops |> List.map ~f:(fun (nums, op) -> nums @ [ op ])

let eval line =
  let to_int =
    List.map ~f:(String.strip ~drop:Char.is_whitespace >> Int.of_string)
  in
  let op, init, numbers =
    match List.rev line with
    | o :: tl when String.contains o '+' -> (( + ), 0, to_int tl)
    | o :: tl when String.contains o '*' -> (( * ), 1, to_int tl)
    | _ -> failwith "Unknown operation"
  in
  List.fold ~init ~f:op numbers

let solve read filepath =
  read filepath
  |> List.fold_left ~init:0 ~f:(fun acc column -> acc + eval column)

let solve1 = solve parse_input_p1
let solve2 = solve parse_input_p2
