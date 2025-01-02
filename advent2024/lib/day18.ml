open Core

module Position = struct 
  type t = {x: int; y: int;} [@@deriving sexp, compare, equal]

  let show t = Printf.sprintf "Position(x=%d;y=%d)" t.x t.y

  let left t = {t with x = t.x - 1}
  let right t = {t with x = t.x + 1}
  let down t = {t with y = t.y + 1}
  let up t = {t with y = t.y - 1}

  let distance a b = Int.abs (a.x - b.x) + Int.abs (a.y + b.y)

  let valid dim t = 
    (t.x < dim.x)
    && (t.x >= 0)
    && (t.y < dim.y)
    && (t.y >= 0)

  let neighbors dim t = 
    [left t; right t; down t; up t;]
    |> List.filter ~f:(valid dim) 

end

module PositionSet = Set.Make(Position)

module Grid = struct

  type value = Empty | Wall

  type t = {dim: Position.t; walls: PositionSet.t}

  let neighbors t p = 
    Position.neighbors t.dim p 
    |> List.filter ~f:(fun p -> not (Set.mem t.walls p))

  let get_all_positions (t:t) = 
    let open Utils in 
    List.map (0--(t.dim.x-1)) ~f:(fun x -> List.map (0--(t.dim.y-1)) ~f:(fun y -> Position.{x;y;}))
    |> List.concat
    |> List.filter ~f:(fun p -> not (Set.mem t.walls p))

end

let read n dimx dimy filepath = 
  let walls = 
    List.take (In_channel.read_lines filepath) n 
    |> List.map ~f:(fun line -> 
      match Re.matches ((Re.rep1 Re.digit) |> Re.compile) line with
      | [x;y;] -> Position.{x=int_of_string x;y = int_of_string y;}
      | _ -> failwith "incorrect input"
    )
    |> PositionSet.of_list
    in
  Grid.{dim=Position.{x=dimx;y=dimy;}; walls = walls}

module Elt = struct
   type t = int * Position.t [@@deriving sexp, equal, compare]
   let compare (a,c) (b,d) = 
    if Int.compare a b  = 0 then Position.compare c d else Int.compare a b
end

module PairSet = struct 
  include Set.Make(Elt)
  let from_positions positions = 
    List.fold positions ~init:empty ~f:(fun acc p -> 
      match p with 
      | Position.{x=0;y=0} -> Set.add acc (0, p)
      | _ -> Set.add acc (Int.max_value, p)
    )
end

let dijkstra (grid: Grid.t) = 
  let all_positions = Grid.get_all_positions grid in 
  let unvisited = PositionSet.of_list all_positions in 
  let q = PairSet.from_positions all_positions in 
  let target = Position.{x=grid.dim.x-1;y=grid.dim.y-1;} in

  let rec aux q unvisited start (distance:int) =
    if Position.equal start target then distance
    else if Set.is_empty q then distance
    else (
(*       Set.iter unvisited ~f:(fun p -> Printf.printf "%s" (Position.show p)); *)
      let neighbors = Grid.neighbors grid start |> List.filter ~f:(Set.mem unvisited) in 
      let q = List.fold neighbors ~init:q ~f:(fun acc n -> Set.add acc (distance + 1, n)) in 
      let q = Set.remove q (distance, start) in 
      let (distance, position) = Set.min_elt_exn q in 
      let unvisited = Set.remove unvisited start in 
      let q = Set.remove q (distance, position) in 
      aux q unvisited position distance 
    )
  
  in 
  aux q unvisited Position.{x=0;y=0;} 0

let solve_p2 n dimx dimy filepath = 
  let length = List.length (In_channel.read_lines filepath) in 
  let rec aux n = 
    let grid = read n dimx dimy filepath in 
    let soln = dijkstra grid in 
    Out_channel.flush Out_channel.stdout;
    if n > length then 
      print_endline "no blocking"
    else if (Int.abs soln) > 1000000000 then
      List.nth_exn (In_channel.read_lines filepath) (n-1)
      |> print_endline
    else
      aux (n+1)
  in 
  aux n

let solve_p1 grid = 
  dijkstra grid
  |> Printf.printf "%d"


