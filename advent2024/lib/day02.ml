open Core
open Re

type direction = | Increasing | Decreasing
let within_tolerance diff = diff >=1 && diff <= 3
let difference direction a b = match direction with | Increasing -> b-a | Decreasing -> a - b
let safe_difference direction a b = difference direction a b |> within_tolerance

let rec aux direction damps report read = 
  match report with 
  | [] | _ :: [] -> true
  | a:: b :: tail when safe_difference direction a b -> aux direction damps (b::tail) (a::read)
  | a :: b :: tail when damps > 0 -> (
    let continue_with n = aux direction (damps-1) (n::tail) read in 
    let can_remove_a = match read with | [] -> true | last::_ -> safe_difference direction last b
    in 
    continue_with a || (can_remove_a && continue_with b)
  )
  | _ -> false

let is_safe damps = function 
    | [] | [_] -> true
    | report -> aux  Increasing damps report [] || aux Decreasing damps report []

let read_reports filepath = 
  let re_digits = 
    (Re.rep1 digit) 
    |> Re.compile 
  in 
  In_channel.read_lines filepath
  |> List.map ~f:(fun line -> Re.matches re_digits line |> List.map ~f:Int.of_string)


let solve_p1 filepath = 
  filepath
  |> read_reports
  |> List.filter ~f:(is_safe 0)
  |> List.fold_left ~f:(fun total _ -> total + 1) ~init:0 
  |> (Printf.printf "%i")

let solve_p2 filepath = 
  filepath
  |> read_reports
  |> List.filter ~f:(is_safe 1)
  |> List.fold_left ~f:(fun total _ -> total + 1) ~init:0 
  |> (Printf.printf "%i")


