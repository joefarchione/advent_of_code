open Core
open Utils

module Coords = struct
  type t = {x: int; y: int;}
  let up t n  = {t with x = t.x+n}
  let right t n  = {t with y = t.y+n}
  let down t n  = {t with x = t.x-n}
  let left t n  = {t with y = t.y-n}
  let valid dim t = 
    (t.x < dim.x)
    && (t.x >= 0)
    && (t.y < dim.y)
    && (t.y >= 0)
end

module CoordsVec = struct 
  type t = Coords.t list
  let north t = List.map (0--3) ~f:(Coords.up t)
  let east t = List.map (0--3) ~f:(Coords.right t)
  let south t = List.map (0--3) ~f:(Coords.down t)
  let west t = List.map (0--3) ~f:(Coords.left t)
  let south_east t = List.map (0--3) ~f:(fun n -> Coords.down (Coords.right t n) n)
  let north_east t = List.map (0--3) ~f:(fun n -> Coords.up (Coords.right t n) n)
  let north_west t = List.map (0--3) ~f:(fun n -> Coords.up (Coords.left t n) n)
  let south_west t = List.map (0--3) ~f:(fun n -> Coords.down (Coords.left t n) n)

  let valid dim t = List.for_all t ~f:(Coords.valid dim)

  let all_positions_from_coords coords =
    [
      north coords;
      east coords;
      south coords;
      west coords;
      north_west coords;
      south_west coords;
      south_east coords;
      north_east coords;
    ]

  let x_mas_positions t = 
    let d1 = [
      Coords.up (Coords.left t 1) 1;
      t;
      Coords.down (Coords.right t 1) 1;
    ] in 
    let d2 = [
      Coords.up (Coords.right t 1) 1;
      t;
      Coords.down (Coords.left t 1) 1;
    ] in 
    [d1; d2]
end

module WordSearch = struct 

  type t = {chars: char list list; dim: Coords.t}

  let create chars =
    let dim_x = List.length chars in 
    let dim_y = match List.hd chars with | Some (l) -> List.length l | None -> failwith "matrix empty" in 
    let dim = Coords.{x=dim_x;y=dim_y} in 
    {chars = chars; dim = dim}

  let get_all_indexes (t:t) = 
    List.map (0--(t.dim.x-1)) ~f:(fun x -> List.map (0--(t.dim.y-1)) ~f:(fun y -> Coords.{x;y;})) 
    |> List.concat 

  let get_char (t:t) (c: Coords.t) : char = 
    match List.nth t.chars c.x with
    | Some (row) -> (
      match List.nth row c.y with
      | Some (letter) -> letter
      | None -> '.'
    )
    | None -> '.'
  
  let get_chars (t:t) (coords_vec: CoordsVec.t) = List.map coords_vec ~f:(get_char t)

  let is_xmas chars = 
    match chars with 
    | ['X'; 'M'; 'A'; 'S'] ->  true
    | _ -> false

  let is_x_mas chars = 
    match chars with 
    | ['M'; 'A'; 'S']  
    | ['S'; 'A'; 'M'] ->  true
    | _ -> false
    
  let is_x c = Char.equal c 'X'
  let is_a c = Char.equal c 'A'

  let check_for_xmas (t:t) position = get_chars t position |> is_xmas |> (fun b -> if b then 1 else 0)

  let check_for_x_mas (t:t) index = 
    if (get_char t index) |> is_a then (
      index
      |> CoordsVec.x_mas_positions
      |> List.for_all ~f:(fun p -> (get_chars t p) |> is_x_mas)
      |> (fun b -> if b then 1 else 0)
    )
    else 0

  let search_for_xmas (t:t) = 
    let indexes = get_all_indexes t |> List.filter ~f:(fun index -> get_char t index |> is_x) in 

    List.map indexes ~f:(fun index -> 
      CoordsVec.all_positions_from_coords index
      |> List.map ~f:(check_for_xmas t)
    |> List.fold_left ~f:(fun acc x -> acc + x) ~init:0
    )
    |> List.fold_left ~f:(fun acc x -> acc + x) ~init:0

  let search_for_x_mas (t:t) = 
    get_all_indexes t
    |> List.map ~f:(check_for_x_mas t)
    |> List.fold_left ~f:(fun acc x -> acc + x) ~init:0

end

let read_input filepath = 
  In_channel.read_lines filepath
  |> List.map ~f:explode
