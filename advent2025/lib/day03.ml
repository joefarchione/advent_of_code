open! Core
open Utils

let read filepath =
  In_channel.read_lines filepath
  |> List.map ~f:(fun l -> String.to_list l |> List.map ~f:Char.get_digit_exn)

let find_largest_joltage bank =
  let rec aux bank tens ones =
    match bank with
    | a :: b :: tl ->
        let tens, ones =
          if a > tens then (a, b)
          else if b > ones then (tens, b)
          else (tens, ones)
        in
        aux (b :: tl) tens ones
    | b :: tl ->
        let ones = if b > ones then b else ones in
        aux tl tens ones
    | _ -> (tens * 10) + ones
  in
  aux bank 0 0

let find_largest_joltage_n n bank =
  let rec aux bank n acc =
    match bank with
    | [] -> acc
    | _ when n = List.length bank -> List.rev bank @ acc
    | _ when n > 0 ->
        let next_digit, next_index =
          List.length bank - n + 1
          |> List.take bank
          |> List.max_elt_i ~compare:Int.compare
          |> Option.value_exn
        in
        let rem_bank = List.drop bank (next_index + 1) in
        aux rem_bank (n - 1) (next_digit :: acc)
    | _ -> acc
  in
  aux bank n []
  |> List.mapi ~f:(fun i x -> x * Int.pow 10 i)
  |> List.fold ~init:0 ~f:( + )

let solve1 filepath =
  read filepath
  |> List.map ~f:find_largest_joltage
  |> List.fold ~init:0 ~f:( + )

let solve2 filepath =
  read filepath
  |> List.map ~f:(find_largest_joltage_n 12)
  |> List.fold ~init:0 ~f:( + )
