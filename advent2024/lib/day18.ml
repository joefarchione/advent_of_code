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

  let get_all_positions dimx dimy = 
    let open Utils in 
    List.map (0--(dimx-1)) ~f:(fun x -> List.map (0--(dimy-1)) ~f:(fun y -> {x;y;}))
    |> List.concat

end

module PositionSet = Set.Make(Position)

module Grid = struct

  type value = Empty | Wall

  type t = {dim: Position.t; walls: PositionSet.t}

  let neighbors t p = 
    Position.neighbors t.dim p 
    |> List.filter ~f:(fun p -> not (Set.mem t.walls p))

end


module PositionDistance = struct 
  module M = Map.Make(Position)
  include M

  type t = int M.t

  let update t position distance = 
    Map.update t position ~f:(function
      | Some (v) -> if distance < v then distance else v
      | None -> distance
    ) 
end 

module PositionPairingHeap = struct

  module Elt = struct
    type t = int * Position.t

    let compare (a, x) (b, y) = 
      match Int.compare a b with 0 -> Position.compare x y | x -> x

    let sexp_of_t (a, x) =
      Sexp.List [ Int.sexp_of_t a; Position.sexp_of_t x ]
  end

  module EltComp = struct
      include Comparator.Make (Elt)
      type t = Elt.t
  end

  type t = Set.M(EltComp).t

  let empty : t = Set.empty (module EltComp)
  let is_empty = Set.is_empty
  let add q ~prio elt = Set.add q (prio, elt)
  let min_elt_exn q = snd (Set.min_elt_exn q)

  let min_elt (q:t) = 
    match Set.min_elt q with 
    | Some (v) -> Some (snd v)
    | None -> None

  let remove (q:t) p = Set.remove q p

  let remove_min_elt_exn q =
    let ((_, elt) as min) = Set.min_elt_exn q in
    (Set.remove q min, elt)

  let remove_min_elt (q:t) =
    match Set.min_elt q with 
    | Some (priority, position) -> Some (remove q (priority, position), (priority, position))
    | None -> None

  let sexp_of_t q =
    let elts = Set.to_list q in
    Sexp.List (List.map elts ~f:Elt.sexp_of_t)

  let get q position = Set.find q ~f:(fun (_, p) -> Position.equal p position)
  let neighbors (q:t) grid position = 
    Grid.neighbors grid position
    |> List.map ~f:(fun p -> get q p)
    |> List.fold ~init:[] ~f:(fun acc p ->
      match p with
      | Some (v) -> v::acc
      | None -> acc
    )


  let from_grid (grid: Grid.t) =
    Position.get_all_positions grid.dim.x grid.dim.y
    |> List.fold ~init:empty ~f:(fun acc p -> 
      if Position.equal p Position.{x=0;y=0;} then 
        add acc ~prio:0 p
      else
        add acc ~prio:Int.max_value p)
end

let read dimx dimy filepath = 
  let walls = 
    In_channel.read_lines filepath
    |> List.map ~f:(fun line -> 
      match Re.matches ((Re.rep1 Re.digit) |> Re.compile) line with
      | [x;y;] -> Position.{x=int_of_string x;y = int_of_string y;}
      | _ -> failwith "incorrect input"
    )
    |> PositionSet.of_list
    in
  Grid.{dim=Position.{x=dimx;y=dimy;}; walls = walls}

let djikstra (grid: Grid.t) = 
  let target = Position.{x=grid.dim.x-1; y = grid.dim.y-1} in 

  let rec aux (unvisited: PositionPairingHeap.t) =  
    match PositionPairingHeap.remove_min_elt unvisited with 
    | Some ((unvisited, (priority, position))) -> (
      if Position.equal position target then priority
      else (
        let neighbors = PositionPairingHeap.neighbors unvisited grid position in 
        let unvisited = List.fold ~init:unvisited neighbors ~f:(fun acc (curr_priority, p) -> 
          Set.add acc (if (priority + 1) < curr_priority then (priority + 1, p) else (curr_priority, p))
        ) in 
        aux unvisited
      )
    )
    | None -> failwith "target unreachable"
  in
  let unvisited = PositionPairingHeap.from_grid grid in 
  aux unvisited


let solve_p1 grid = 
  djikstra grid
  |> Printf.printf "%d"

