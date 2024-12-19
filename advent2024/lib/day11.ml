open Core

module Stone = struct
  type t = int

  let split_even_number n =
    let len = String.length (string_of_int n) in
    let half_len = len / 2 in
    let divisor = Int.pow 10 half_len in
    let first_half = n / divisor
    and second_half = n mod divisor in
    (first_half, second_half)

  let count_digits n =
    let rec aux n acc =
      if Int.equal n 0 then acc
      else aux (n / 10) (acc + 1)
    in
    aux n 0

  let is_zero t = Int.equal t 0
  let is_even num = Int.equal (num mod 2) 0
  let even_number_of_digits t = t |> count_digits |> is_even

end

module Blinker = struct
  module Key = struct type t = int * int [@@deriving sexp, compare, equal, hash] end
  module StoneMemo = Hashtbl.Make(Key)

  open Stone

  type blink_result = | One of int | Split of (int * int)

  let blink stone: (blink_result) = 
    if is_zero stone then 
      One 1
    else if even_number_of_digits stone then (
      Split (split_even_number stone)
    )
    else
      One (stone*2024)
      
  let calculate_num_stones n stones = 
    let memo = StoneMemo.create ~growth_allowed:true ~size:5000 () in 
    let blink_n n stone = 
      let rec blink_n' n stone = 
        match Hashtbl.find memo (stone, n) with 
        | Some (v) -> v
        | None -> (
          let num_stones = 
            if Int.equal n 0 then 1
            else (
              match blink stone with 
              | One (s) -> blink_n' (n-1) s
              | Split (s1, s2) -> 
                (blink_n' (n-1) s1) + (blink_n' (n-1) s2)
            )
          in 
          Hashtbl.add_exn memo ~key:(stone, n) ~data:num_stones;
          num_stones
        )
      in
      blink_n' n stone
    in
    List.fold stones ~init:0 ~f:(fun acc stone -> (blink_n n stone) + acc)

end

let read (filepath: string) = 
  filepath
  |> In_channel.read_lines 
  |> List.hd
  |> (function 
      | Some (l) -> (
        Re.matches (Re.rep1 Re.digit |> Re.compile) l 
        |> List.map ~f:(fun x -> int_of_string (x))
      )
      | None -> failwith "empty file"
  )
  
let solve_p1 stones = 
  Blinker.calculate_num_stones 25 stones
  |> Printf.printf "%d"

let solve_p2 stones = 
  Blinker.calculate_num_stones 75 stones
  |> Printf.printf "%d"

