open Core
open Utils

module AntennaMap = Map.Make(Char)

type input = {map: Utils.Coords.t list AntennaMap.t; dim: Utils.Coords.t}

let read filepath = 
  let dim = Utils.Coords.dimension_from_file filepath in
  let mapping = In_channel.read_lines filepath
    |> List.foldi ~init:AntennaMap.empty
      ~f:(fun i acc line ->
          List.foldi (String.to_list line) ~init:acc
          ~f:(fun j map c ->
              let trans = Coords.{x = j; y = dim.y - i - 1} in 
              Map.update map c ~f:(function | Some (v) -> trans :: v | None -> [trans] )
            )
        )
    in 
  {map=mapping; dim=dim;}

let find_antinodes_p1 (a: Coords.t) (b:Coords.t) =
  if a.x = b.x then (
    let dt = Int.abs (a.y - b.y) in 
    let ymin = Int.min a.y b.y in 
    let ymax = Int.max a.y b.y in
    (Coords.{x=a.x;y=ymin - dt}, Coords.{x=a.x;y=ymax + dt})
  )
  else (
    let slope = Coords.slope a b in 
    let intercept = float_of_int (a.y) -. slope *. float_of_int (a.x)in 
    let xmin = Int.min a.x b.x in 
    let xmax = Int.max a.x b.x in
    let dt = Int.abs (a.x - b.x) in 
    let a1 = slope *. float_of_int (xmin-dt) +. intercept in 
    let a2 = slope *. float_of_int  (xmax+dt) +. intercept in 

(*     Printf.printf "Antinodes for a=%s, b=%s\n" (Coords.show a) (Coords.show b);
    Printf.printf "\t%f = %f * (%d - %d) + %f\n" a1 slope xmin dt intercept;
    Printf.printf "\t%f = %f * (%d - %d) + %f\n" a1 slope xmax dt intercept; *)

    (Coords.{x=xmin-dt;y=int_of_float (Float.round a1)}, Coords.{x=xmax+dt;y=int_of_float (Float.round a2)})
  )

let find_antinodes_p2 dim (a: Coords.t) (b:Coords.t) =
  if a.x = b.x then (
    let dt = Int.abs (a.y - b.y) in 
    let ymin = Int.min a.y b.y in 
    let ymax = Int.max a.y b.y in
    
    let rec move x0 dt points = 
      let next_point = Coords.{x=a.x;y=x0 + dt} in 
      if Coords.valid dim next_point then 
        move (x0+dt) dt (next_point::points)
      else
        points
      in
    let points = move ymin (~-dt) [] |> move ymax dt in 
    a::b::points
  )
  else (
    let slope = Coords.slope a b in 
    let intercept = float_of_int (a.y) -. slope *. float_of_int (a.x)in 
    let xmin = Int.min a.x b.x in 
    let xmax = Int.max a.x b.x in
    let dt = Int.abs (a.x - b.x) in 

    let rec move x0 dt points = 
      let a1 = slope *. float_of_int (x0+dt) +. intercept in 
      let next_point = Coords.{x=x0+dt;y=int_of_float (Float.round a1)} in 
      if Coords.valid dim next_point then 
        move (x0+dt) dt (next_point::points)
      else
        points
    in

    let points = move xmin (~-dt) [] in 
    let points = move xmax dt points in 
    (a::b::points)
  )

let find_valid_antinodes dim a b = 
  let (a1, a2) = find_antinodes_p1 a b in 
  [a1; a2;]
  |> List.filter ~f:(Coords.valid dim)

module CoordsSet = Set.Make(Coords)

let find_valid_antinodes_p2 dim a b = 
  find_antinodes_p2 dim a b
  |> List.filter ~f:(Coords.valid dim)

let find_all_valid_antinodes_p1 (dim: Coords.t) (coords: Coords.t list) = 
  Utils.subsets_of_size_2 coords 
  |> List.map ~f:(fun (a,b) -> find_valid_antinodes dim a b)
  |> List.concat
  |> CoordsSet.of_list

let find_all_valid_antinodes_p2 (dim: Coords.t) (coords: Coords.t list) = 
  Utils.subsets_of_size_2 coords 
  |> List.map ~f:(fun (a,b) -> find_valid_antinodes_p2 dim a b)
  |> List.concat
  |> CoordsSet.of_list

let solve_p1 input = 
  input.map
  |> Map.fold ~init:CoordsSet.empty ~f:(fun ~key:key ~data:coords acc -> 
    if Char.equal key '.' then 
      acc
    else
     let anitnodes = find_all_valid_antinodes_p1 input.dim coords in 
     Set.union acc anitnodes
  )
  |> Set.length
  |> Printf.printf "%d"

let solve_p2 input = 
  input.map
  |> Map.fold ~init:CoordsSet.empty ~f:(fun ~key:key ~data:coords acc -> 
    if Char.equal key '.' then 
      acc
    else
     let anitnodes = find_all_valid_antinodes_p2 input.dim coords in 
     Set.union acc anitnodes
  )
  |> Set.length
  |> Printf.printf "%d"