open Core

module Direction = struct
  type t = | Up | Right | Left | Down [@@deriving sexp, compare, equal]

  let rotate t = 
    match t with 
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

end

module Position = struct
  type t = {x: int; y: int} [@@deriving sexp, compare, equal]
end

module PositionSet = Set.Make(Position)

module Dimension = struct
  type t  = {x:int; y: int}

  let within (dim: t) (position: Position.t) = 
    (position.x < dim.x)
    && (position.x >= 0)
    && (position.y < dim.y)
    && (position.y >= 0)

  let get (values: 'a array array) = {x = Array.length values; y = Array.length values.(0)}

  let get_all_positions t = 
    let open Utils in 
    List.map (0--t.x) ~f:(fun x -> List.map (0--t.y) ~f:(fun y -> Position.{x;y}))
    |> List.concat
end

module CharMatrix  = struct

  type t  = {values: char array array; dim: Dimension.t}

  let create chars = {values=chars; dim = Dimension.get chars}

  let copy t = {values = Array.map t.values ~f:(fun r -> Array.copy r) |> Array.copy; dim=t.dim}

  let from_list ll = 
    let arr = Utils.list_to_matrix ll in 
    {values= arr; dim = Dimension.get arr}

  let get t x y = 
    try 
      Some (t.(x).(y))
    with
      | Invalid_argument _ -> None

  let find_index predicate (matrix: t) =
    let (rows, cols) = (matrix.dim.x, matrix.dim.y) in 
    let rec find_in_row row col =
      if row >= rows then
        None
      else if col >= cols then
        find_in_row (row + 1) 0
      else if predicate matrix.values.(row).(col) then
        Some (row, col)
      else
        find_in_row row (col + 1)
    in
    find_in_row 0 0

end

module Point = struct 
  type tag = | Obstacle | Nothing | Guard
  type t = {value: char; position: Position.t; tag: tag;}
  let of_char = function | '#' -> Obstacle | '^' -> Guard  | '.' -> Nothing | _ -> failwith "Incorrect characterr"
  let is_guard =  Char.equal '^'

  let from_position t (p: Position.t) = 
    let value = CharMatrix.get t p.x p.y in 
    match value with 
    | Some (v) -> Some {value=v; position=p;tag=of_char v;}
    | None -> None

end

module Guard = struct 
  type t = {position: Position.t; direction: Direction.t;} [@@deriving sexp, compare, equal]
  let rotate guard = {guard with direction = Direction.(rotate guard.direction)}
end 

module GuardSet = Set.Make(Guard)

module Map = struct 
  type t = CharMatrix.t

  let next_point (t:t) (guard: Guard.t) = 
    let open Direction in 
    match guard.direction with 
    | Up -> Point.from_position t.values {guard.position with x = guard.position.x - 1}
    | Down -> Point.from_position t.values {guard.position with x = guard.position.x + 1}
    | Left -> Point.from_position t.values {guard.position with y = guard.position.y - 1}
    | Right -> Point.from_position t.values {guard.position with y = guard.position.y + 1}

  let find_initial_guard t = 
    match CharMatrix.find_index Point.is_guard t with 
    | Some ((x,y)) -> Some Guard.{position=Position.{x;y}; direction=Direction.Up}
    | None -> None

  let rec move_position (t:t) (guard: Guard.t) = 
    let point = next_point t guard in 
    match point with
    | Some (p) -> (
      match p with 
      | Point.{tag=Obstacle; _} -> Guard.rotate guard |> move_position t
      | Point.{tag=Nothing; _} | Point.{tag=Guard;_}-> Some {guard with position = p.position}
    )
    | None -> None

  let change_to_nothing (t:t) (position: Position.t) = 
    let point = Point.from_position t.values position in 
    match point with 
    | Some (_) -> 
      t.values.(position.x).(position.y) <- '.';
      t
    | _ -> t

  let change_to_obstacle (t:t) (position: Position.t) = 
    let point = Point.from_position t.values position in 
    let open Point in 
    match point with 
    | Some {tag=Guard; _} | Some {tag=Obstacle;_} | None -> None
    | _ -> (
      t.values.(position.x).(position.y) <- '#';
      Some t
    )

end

let solve_p1 filepath = 
  let t  = Utils.read_input filepath |> CharMatrix.from_list in
  let open Map in
  let rec aux t guard distinct_positions = 
    match move_position t guard with 
    | Some (g) -> (
      aux t g (Set.add distinct_positions g.position)
    )
    | None -> distinct_positions
  in
  let value = (
    match find_initial_guard t with 
    | Some (g) -> (
      aux t g (Set.add PositionSet.empty g.position) |> Set.length
    )
    | None -> 0
  ) in 

  Printf.printf "%d" value;;

let has_a_loop (t: CharMatrix.t) guard = 
  let open Map in
  let rec aux t guard distinct_guards = 
    match move_position t guard with 
    | Some (g) -> (
      if Set.mem distinct_guards g then 
        1
      else (
        aux t g (Set.add distinct_guards g)
      )
    )
    | None -> 0
  in
  aux t guard (Set.add GuardSet.empty guard)

let get_all_guard_positions (t: CharMatrix.t) (initial_guard: Guard.t) = 
  let open Map in
  let rec aux t guard distinct_positions = 
    match move_position t guard with 
    | Some (g) -> (
      aux t g (Set.add distinct_positions g.position)
    )
    | None -> distinct_positions
  in
  (aux t initial_guard PositionSet.empty) |> Set.to_list

let solve_p2 filepath = 
  let t  = Utils.read_input filepath |> CharMatrix.from_list in 
  let open Map in 
  let value = (match find_initial_guard t with 
  | Some (g) -> (
    get_all_guard_positions t g
    |> List.map ~f:(fun pos_to_change -> 
      let new_map = change_to_obstacle (CharMatrix.copy t) pos_to_change in 
      match new_map with 
      | Some (m) -> 
        let result = has_a_loop m g in 
        result
      | None -> 0
    )
    |> List.fold_left ~f:(fun acc a -> acc + a) ~init:0
  )
  | None -> 0) in
  Printf.printf "%d" value;;