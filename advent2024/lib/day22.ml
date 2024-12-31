open Core 
let mix secret number = Int.bit_xor secret number
let prune number = number mod 16777216
let evolve (secret:int) = 
  let secret = (secret * 64) |> mix secret |> prune in 
  let secret = (secret / 32) |> mix secret |> prune in
  let secret = (secret * 2048) |> mix secret |> prune in
  secret
;;

let rec evolve_n n secret = 
  match n with 
  | 0 -> secret
  | _ -> evolve_n (n-1) (evolve secret)

let get_last_digit (n:int) = (String.suffix (string_of_int n) 1) |> int_of_string

let evolve_last_digit_n n secret = 
  let rec aux n secret last_digit lst = 
    match n with 
    | 0 -> lst
    | _ -> (
      let next_secret = evolve secret in 
      let next_last_digit = get_last_digit next_secret in
      aux (n-1) next_secret next_last_digit ((next_last_digit, next_last_digit - last_digit)::lst)
    )
  in
  aux n secret (get_last_digit secret) [(secret, 0)]

module ChangeSequence = struct type t = int * int * int * int [@@deriving sexp, equal, compare] end
module ChangeMap = Map.Make(ChangeSequence)

let group_change_sequences (digit_change_lst: (int * int) list) = 
  let rec aux (digit_change_lst: (int * int) list) mapping = 
    match digit_change_lst with
    | a::b::c::d::tl -> (
      let mapping = Map.update mapping (snd a, snd b, snd c, snd d) ~f:(fun _ -> fst a) in 
      aux (b::c::d::tl) mapping
    )
    | _ -> mapping
  in
  aux digit_change_lst ChangeMap.empty

let merge_maps total_map line_map = 
  Map.fold line_map ~init:total_map ~f:(fun ~key:k ~data:v acc -> 
    Map.update acc k ~f:(function | Some (total) -> total + v | None -> v)
  )

let read filepath = 
  In_channel.read_lines filepath 
  |> List.map ~f:Int.of_string

let solve_p1 numbers = 
  numbers 
  |> List.map ~f:(evolve_n 2000)
  |> List.fold ~init:0 ~f:(+)
  |> Printf.printf "%d"

let solve_p2 numbers = 
  List.fold numbers ~init:ChangeMap.empty ~f:(fun acc number -> 
    let digit_change_lst = evolve_last_digit_n 2000 number in 
    let line_mapping = group_change_sequences digit_change_lst in 
    merge_maps acc line_mapping
  )
  |> Map.fold ~init:0 ~f:(fun ~key:_ ~data:v acc -> if v < acc then acc else v)
  |> Printf.printf "%d"