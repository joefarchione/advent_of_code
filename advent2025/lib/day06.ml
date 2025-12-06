open! Core
open Functions

let split_line string = Str.split (Str.regexp "[ \t]+") string

let parse_input_p1 filepath =
  In_channel.read_lines filepath |> List.map ~f:split_line |> List.zip_n

let parse_input_p2 filepath =
  let ops, numbers =
    In_channel.read_lines filepath |> List.rev |> List.hd_tl_exn
    |> fun (a, b) -> (a, List.rev b)
  in
  let ops = split_line ops in
  let numbers =
    numbers |> List.map ~f:String.to_list |> List.transpose_exn
    |> List.map ~f:String.of_list
    |> List.split (String.for_all ~f:Char.is_whitespace)
    |> List.rev
  in
  List.zip_exn numbers ops |> List.map ~f:(fun (nums, op) -> nums @ [ op ])

let eval parse line =
  let op, init, numbers =
    match List.rev line with
    | o :: tl when String.contains o '+' -> (( + ), 0, parse tl)
    | o :: tl when String.contains o '*' -> (( * ), 1, parse tl)
    | _ -> failwith "Unknown operation"
  in
  List.fold ~init ~f:op numbers

let solve read filepath =
  let to_int =
    List.map ~f:(fun s ->
        s |> String.strip ~drop:(Char.equal ' ') |> Int.of_string)
  in

  read filepath
  |> List.fold_left ~init:0 ~f:(fun acc column -> acc + eval to_int column)

let solve1 = solve parse_input_p1
let solve2 = solve parse_input_p2
