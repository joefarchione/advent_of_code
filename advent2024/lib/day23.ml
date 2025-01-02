open! Core

type computer = string

module Connection = struct type t = string * string [@@deriving compare, equal, sexp] end
module Clique = struct type t = string list [@@deriving compare, sexp] end
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

let read_computers filepath = 
  In_channel.read_lines filepath
  |> List.fold ~init:ComputerSet.empty ~f:(fun acc line -> 
    match (String.split ~on:'-' line) with
    | [a;b] ->  Set.add (Set.add acc a) b
    | _ -> failwith "incorrect input"
  )

let filter_historians computers = 
  computers |> Set.filter ~f:(fun c -> String.equal (String.prefix c 1) "t")

type input = {
  connections: ConnectionSet.t;
  map: ConnectionMap.t;
  computers: ComputerSet.t;
}

let read filepath = 
  let connections = read_connections filepath in
  let map = read_connection_map filepath in 
  let computers = read_computers filepath in 
  {connections; map; computers;}

let solve_p1 input = 
  Set.fold ~init:CliqueSet.empty (filter_historians input.computers)
    ~f:(fun acc historian -> 
      let connected_to = Set.elements (Map.find_exn input.map historian) in
      let all_connections = Utils.cartesian_product connected_to connected_to in 
      List.filter all_connections ~f:(fun c -> Set.mem input.connections c)
      |> List.fold ~init:acc ~f:(fun acc (a,b) -> Set.add acc (ComputerSet.of_list (historian::a::b::[])))
    )
  |> Set.length
  |> Printf.printf "%d"

let can_be_added_to_clique map clique computer = 
  let connections = Map.find_exn map computer in 
  Set.for_all clique ~f:(Set.mem connections)

let solve_p2 input = 
  let minimal_cliques = 
    Set.fold ~init:CliqueSet.empty input.computers 
      ~f:(fun acc computer -> 
        let connected_to = Set.elements (Map.find_exn input.map computer) in
        let all_connections = Utils.cartesian_product connected_to connected_to in 
        List.filter all_connections ~f:(fun c -> Set.mem input.connections c)
        |> List.fold ~init:acc ~f:(fun acc (a,b) -> Set.add acc (ComputerSet.of_list (computer::a::b::[])))
      )
  in

  let rec add_to_clique map computers (clique: ComputerSet.t) = 
    match computers with 
    | computer::tl when can_be_added_to_clique map clique computer -> (
      let next_clique = Set.add clique computer in 
      let c1 = add_to_clique map tl next_clique in 
      let c2 = add_to_clique map tl clique in 
      if Set.length c1 > Set.length c2 then c1 else c2
    )
    | _::tl -> add_to_clique map tl clique
    | [] -> clique
  in 

  CliqueSet.map 
    minimal_cliques 
    ~f:(fun clique -> add_to_clique input.map (Set.elements input.computers) clique)
  |> Set.fold ~init:ComputerSet.empty ~f:(fun acc s -> if Set.length s > Set.length acc then s else acc)
  |> Set.to_list
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","
  |> Printf.printf "%s"

