open Core



type key = int list
type lock = int list
type schematic = Key of key | Lock of lock

let read_schematic schema lines =
  List.map lines ~f:(String.to_list)
  |> Utils.transpose
  |> List.map ~f:(fun chars -> 
      chars
      |> List.filter ~f:(Char.equal '#')
      |> List.length
      |> (fun d -> 
        match schema with
        | `Key -> d - 1
        | `Lock -> d
      )
  )

type input = {keys: key list; locks: lock list;}

let print input = 
  print_endline "Keys";
  List.iter input.keys ~f:(fun k -> List.iter k ~f:(fun c-> (Printf.printf "%d") c); print_endline "";);

  print_endline "Locks";
  List.iter input.locks ~f:(fun k -> List.iter k ~f:(fun c-> (Printf.printf "%d") c); print_endline "";)

let read filepath = 
  In_channel.read_lines filepath
  |> Utils.split_on_newline 
  |> List.fold ~init:{keys=[]; locks=[];} ~f:(fun acc lines -> 
    match lines with 
    | "#####"::tl -> {acc with locks = (read_schematic `Lock tl)::acc.locks}
    | "....."::tl -> {acc with keys = (read_schematic `Key tl)::acc.keys}
    | _ -> failwith "incorrect input"
  )
  |> (fun inputs -> {keys =List.rev inputs.keys; locks = List.rev inputs.locks})

let fits key lock =
  let rec aux key_lock = 
    match key_lock with
    | (k,l)::tl when k + l <= 5 -> aux tl 
    | _::_ -> false
    | [] -> true
  in
  aux (List.zip_exn key lock)

let solve_p1 input = 
  Utils.cartesian_product input.keys input.locks
  |> List.filter ~f:(fun (key, lock) -> fits key lock)
  |> List.length
  |> Printf.printf "%d"
