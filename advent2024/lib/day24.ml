open Core

type op = AND | OR | XOR
let op_of_string s = match s with | "AND" -> AND | "OR" -> OR | "XOR" -> XOR | _ -> failwith "unsupported operation"
type wire = string
type connection = {left: wire; right: wire; op: op; output: wire}

let eval map connection = 
  let left = Map.find_exn map connection.left in 
  let right = Map.find_exn map connection.right in 
  let value = 
    match connection.op with
    | AND -> Int.bit_and left right
    | OR -> Int.bit_or left right
    | XOR -> Int.bit_xor left right
  in
  Map.update map connection.output ~f:(function | Some (_) -> value | None -> value)


module WireMap = struct 
  module M = Map.Make(String) [@@deriving sexp, show, equal]
  include M
end

type input = {map: int WireMap.t; connections: connection list; output_map: connection WireMap.t;}

let read filepath =  
  let open Re in 
  let init_value = seq [group (seq [alpha; rep1 digit; ]); str ": "; group (digit)] |> Re.compile in 
  let map  = seq [
    group (rep1 (alt [alpha; digit])); 
    space; 
    group (alt [str "XOR"; str "OR"; str "AND"]); 
    space; 
    group (rep1 (alt [alpha; digit])); 
    space;
    str "->"; 
    space; 
    group (rep1 (alt [alpha;digit])) ] |> Re.compile in 

  let initial_values = 
    In_channel.read_lines filepath
    |> List.fold ~init:WireMap.empty ~f:(fun acc line -> 
      match Re.exec_opt init_value line with
      | Some (group) -> Map.add_exn acc ~key:(Group.get group 1) ~data:(int_of_string (Group.get group 2))
      | None -> acc
    )
  in

  let connection_list = 
    In_channel.read_lines filepath
    |> List.fold ~init:[] ~f:(fun acc line -> 
        match Re.exec_opt map line with
        | Some (group) -> (
          let left = Group.get group 1 in  
          let op = op_of_string (Group.get group 2) in  
          let right = Group.get group 3 in  
          let output = Group.get group 4 in  
          {left; right; op; output;}::acc
        )
        | None  -> acc
    )
    |> List.rev
  in

  let connection_map = 
    In_channel.read_lines filepath
    |> List.fold ~init:WireMap.empty ~f:(fun acc line -> 
        match Re.exec_opt map line with
        | Some (group) -> (
          let left = Group.get group 1 in  
          let op = op_of_string (Group.get group 2) in  
          let right = Group.get group 3 in  
          let output = Group.get group 4 in  
          Map.add_exn acc ~key:output ~data:{left; right; op; output;}
        )
        |None -> acc
    )
  in
  {map=initial_values; connections=connection_list; output_map = connection_map;}

let is_defined map wire = Map.mem map wire

let rec eval_p1 input c = 
  if (is_defined input.map c.left) && (is_defined input.map c.right) then (
    eval input.map c
  )
  else if (is_defined input.map c.right) then  (
    (*look up connection that defines left*)
    let connection = Map.find_exn input.output_map c.left in 
    let map = eval_p1 input connection in 
    eval map c
  )
  else if (is_defined input.map c.left) then  (
    (*look up connection that defines left*)
    let connection = Map.find_exn input.output_map c.right in 
    let map = eval_p1 input connection in 
    eval map c
  )
  else  (
    (*look up connection that defines left*)
    let c1 = Map.find_exn input.output_map c.right in 
    let c2 = Map.find_exn input.output_map c.left in 
    let map = eval_p1 input c1 in 
    let map = eval_p1 {input with map = map} c2 in 
    eval map c
  )

let rec solve_p1' input = 
  match input.connections with 
  | hd::tl when Map.mem input.map hd.output -> solve_p1' {input with connections = tl}
  | hd::tl -> solve_p1' {input with map = eval_p1 input hd; connections=tl;} 
  | _ -> input

let solve_p1 input = 
  let input = solve_p1' input in 
  Map.to_alist input.map
  |> List.filter ~f:(fun (a,_) -> String.equal (String.prefix a 1) "z")
  |> List.sort ~compare:(fun (a,_) (b,_) -> String.compare a b)
  |> List.map ~f:(fun (_, b) -> b)
  |> List.fold ~init:"" ~f:(fun acc s -> (Int.to_string s) ^ acc)
  |> (fun s -> int_of_string ("0b"^s))
  |> Printf.printf "%d"



