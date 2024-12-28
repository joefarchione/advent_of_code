open! Core

type computer = string

module Connection = struct type t = string * string [@@deriving compare, equal, sexp] end
module Clique = struct type t = string * string * string [@@deriving compare, sexp] end
module ConnectionSet = Set.Make(Connection)
module ComputerSet = Set.Make(String)
module CliqueSet = Set.Make(ComputerSet) 
module ConnectionMap = struct
  module M = Map.Make(String)
  include M
  type t = ComputerSet.t M.t

  let update_add t k d = 
    Map.update t k ~f:(function
      | Some (v) -> Set.add v d
      | None -> Set.add (ComputerSet.empty) d
    )

  let update_both t k d = update_add (update_add t k d) d k 
end

let read_connections filepath = 
  In_channel.read_lines filepath
  |> List.fold ~init:[] ~f:(fun acc line ->
    match String.split ~on:'-' line with
    | [a;b] -> (a,b)::(b,a)::acc
    | _ -> failwith "incorrect input"
  )
  |> ConnectionSet.of_list

let read_connection_map filepath : ConnectionMap.t = 
  In_channel.read_lines filepath
  |> List.fold ~init:ConnectionMap.empty ~f:(fun acc line ->
    match (String.split ~on:'-' line) with
    | [a;b] ->  (ConnectionMap.update_both acc a b)
    | _ -> failwith "incorrect input"
  )

let read_historians filepath = 
  In_channel.read_lines filepath
  |> List.fold ~init:ComputerSet.empty ~f:(fun acc line -> 
    match (String.split ~on:'-' line) with
    | [a;b] ->  Set.add (Set.add acc a) b
    | _ -> failwith "incorrect input"
  )
  |> Set.filter ~f:(fun c -> String.equal (String.prefix c 1) "t")

type input = {
  connections: ConnectionSet.t;
  map: ConnectionMap.t;
  historians: ComputerSet.t;
}

let read filepath = 
  let connections = read_connections filepath in
  let map = read_connection_map filepath in 
  let historians = read_historians filepath in 
  {connections; map; historians;}

let solve_p1 input = 
  Set.fold ~init:CliqueSet.empty input.historians 
    ~f:(fun acc historian -> 
      let connected_to = Set.elements (Map.find_exn input.map historian) in
      let all_connections = Utils.cartesian_product connected_to connected_to in 
      List.filter all_connections ~f:(fun c -> Set.mem input.connections c)
      |> List.fold ~init:acc ~f:(fun acc (a,b) -> Set.add acc (ComputerSet.of_list (historian::a::b::[])))
    )
  |> Set.length
  |> Printf.printf "%d"

