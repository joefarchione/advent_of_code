open Core

let get_prefix s n = String.sub s ~pos:0 ~len:n
let is_prefix prefix string = 
  let n = String.length prefix  in
  let m = String.length string in 
  if m < n then
    false
  else
    String.equal (get_prefix string n) prefix

let remove_prefix prefix string = 
  let n = String.length prefix  in
  String.sub string ~pos:n ~len:(String.length string - n)

type input = {towels: string list; designs: string list}

let read filepath = 
  let lines = In_channel.read_lines filepath in
  match lines with 
  | towels_s::_::designs -> (
    let towels = towels_s
      |> String.filter ~f:(fun c -> not (Char.equal ' ' c))
      |> String.split ~on:',' in 
    {towels;designs}
  )
  | _ -> failwith "incorrect input format"

let is_possible design towels = 
  let rec aux rem_towels design = 
    match rem_towels with 
    | [] -> false
    | hd :: tl when is_prefix hd design -> (
      match (remove_prefix hd design) with 
      | "" -> true
      | s when aux towels s -> true
      | _ -> aux tl design
    )
    | _ :: tl -> aux tl design
  in 
  aux towels design

module DesignCache = Hashtbl.Make(String)

let number_of_ways designs towels = 
  let memo = DesignCache.create () in 
  let rec aux rem_towels design = 
    match Hashtbl.find memo design with 
    | Some (v) -> v
    | None -> 
      if String.is_empty design then 
        1
      else 
        match rem_towels with 
        | towel::tl when is_prefix towel design -> (
          let s = remove_prefix towel design in 
          let num_ways = (aux towels s)  in 
          let num_ways_rest = (aux tl design) in 
          Hashtbl.update memo s ~f:(fun _ -> num_ways);
          num_ways + + num_ways_rest
        )
        | _::tl -> aux tl design
        | [] -> 0

  in 
  List.fold designs ~init:0 ~f:(fun acc design -> (aux towels design) + acc)

let solve_p1 input = 
  List.fold input.designs ~init:0 
    ~f:(fun acc d-> (if is_possible d input.towels then 1 else 0) + acc)
  |> Printf.printf "%d"

let solve_p2 input = 
  number_of_ways input.designs input.towels
  |> Printf.printf "%d" 





