open Aoc

type product_id = char list

let product_id_range (a, b) =
  Sequence.range_inclusive (Int.of_string a) (Int.of_string b)
  |> Sequence.map ~f:Int.to_string

let read filepath =
  In_channel.read_lines filepath
  |> List.hd_exn
  |> String.split_on_chars ~on:[ ',' ]
  |> List.map ~f:(fun range_str ->
         match String.split_on_chars ~on:[ '-' ] range_str with
         | [ a; b ] -> (a, b)
         | _ -> failwith "Invalid range format")

let is_repeated_twice s =
  let re = Pcre2.regexp {|^(\d+)\1$|} in
  (try Some (Pcre2.exec ~rex:re s) with _ -> None) |> Option.is_some

let is_repeated_at_least_twice s =
  let re = Pcre2.regexp {|^(\d+)\1+$|} in
  (try Some (Pcre2.exec ~rex:re s) with _ -> None) |> Option.is_some

let solve predicate filepath =
  read filepath
  |> List.map
       ~f:
         (product_id_range
         >> Sequence.filter ~f:predicate
         >> Sequence.map ~f:Int.of_string
         >> Sequence.fold ~f:( + ) ~init:0)
  |> List.fold ~init:0 ~f:( + )

let solve1 filepath = solve is_repeated_twice filepath
let solve2 filepath = solve is_repeated_at_least_twice filepath
