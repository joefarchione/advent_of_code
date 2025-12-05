open! Core
open Functions

module Range = struct
  type t = int * int [@@deriving compare, sexp, hash]

  let length (a, b) = b - a + 1
  let overlapping (a, b) (x, y) = a <= x && x <= b && b <= y
  let contained_in (a, b) (x, y) = a <= x && y <= b

  let sort (a, b) (x, y) =
    match Int.compare a x with 0 -> Int.compare b y | c -> c

  let contains (a, b) x = a <= x && x <= b
end

module RangeList = struct
  include List

  type t = Range.t list [@@deriving compare, sexp, hash]

  let sort = List.sort ~compare:Range.sort

  let is_fresh t ingredient =
    List.exists t ~f:(fun range -> Range.contains range ingredient)

  let merge ranges =
    let rec aux ranges acc =
      match ranges with
      | [] -> List.rev acc
      | a :: [] -> List.rev (a :: acc)
      | a :: b :: tl ->
          if Range.overlapping a b then
            let merged = (fst a, snd b) in
            aux (merged :: tl) acc
          else if Range.contained_in a b then aux (a :: tl) acc
          else aux (b :: tl) (a :: acc)
    in
    aux ranges []
end

let read filepath =
  let ranges, ingredients =
    In_channel.read_lines filepath |> List.split_on ~on:(String.equal "")
  in
  let ranges =
    List.map ranges ~f:(fun line ->
        match String.split ~on:'-' line with
        | [ min; max ] -> (Int.of_string min, Int.of_string max)
        | _ -> failwith "Invalid range format")
  in
  let ingredients = List.map ingredients ~f:Int.of_string in
  (ranges, ingredients)

let solve1 filepath =
  let ranges, ingredients = read filepath in
  ingredients
  |> RangeList.filter ~f:(RangeList.is_fresh ranges)
  |> RangeList.length

let solve2 filepath =
  read filepath |> fst |> RangeList.sort |> RangeList.merge
  |> RangeList.map ~f:Range.length
  |> RangeList.cumsum (module Int)
