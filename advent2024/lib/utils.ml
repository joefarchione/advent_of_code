open! Core

let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j [] ;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let list_to_matrix ll = Array.of_list (List.map ~f:Array.of_list ll)

let read_input filepath = 
  In_channel.read_lines filepath
  |> List.map ~f:explode


let rec permutations (n:int) (lst:'a list) : 'a list list = 
  let append x items = List.map ~f:(fun xs -> x::xs) items in  
  match n with
  | 0 -> []
  | 1 -> List.map ~f:(fun x -> [x]) lst
  | _ -> List.map ~f:(fun v -> append v (permutations (n - 1) lst)) lst |> List.concat

let rec elem_wise_mult list1 list2 =
  match list1, list2 with
  | [], [] -> []
  | x::xs, y::ys -> (x * y) :: (elem_wise_mult xs ys)
  | _, _ -> failwith "Lists must have the same length"


let split_digits n =
  let rec split_digits' n = 
    if n = 0 then []
    else (n mod 10) :: (split_digits' (n / 10))
  in
  if n < 10 then [n]
  else split_digits' n |> List.rev

let split_list list predicate =
  let rec aux acc1 acc2 = function
    | [] -> List.rev acc1, List.rev acc2
    | x::xs -> if predicate x then aux (x::acc1) acc2 xs else aux acc1 (x::acc2) xs
  in
  aux [] [] list

module Coords = struct
  type t = {x: int; y: int;} [@@deriving sexp, equal, compare]
  let up_n n t  = {t with x = t.x+n}
  let right_n n t  = {t with y = t.y+n}
  let down_n n t  = {t with x = t.x-n}
  let left_n n t  = {t with y = t.y-n}

  let up  = up_n 1
  let right  = right_n 1
  let down  = down_n 1
  let left  = left_n 1

  let valid dim t = 
    (t.x < dim.x)
    && (t.x >= 0)
    && (t.y < dim.y)
    && (t.y >= 0)

  let distance a b = Int.(abs (a.x - b.x) + abs (a.y - b.y))

  let next dim t = 
    [up t; down t; right t; left t;]
    |> List.filter ~f:(fun p -> valid dim p)

end

module Grid = struct
  module CoordsSet = Set.Make(Coords)
  module DistanceMap = Map.Make(Coords)

end

let memo_rec f =
  let m = ref [] in
  let rec g x =
    match List.Assoc.find (!m) ~equal:(Poly.equal) x with 
    | Some (v) -> v
    | None -> (
      let y = f g x in
        m := (x, y) :: !m ;
        y
    )
  in
    g