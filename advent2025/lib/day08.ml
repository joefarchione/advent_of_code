open Aoc

let distance (a, b, c) (x, y, z) =
  Int.pow (a - x) 2 + Int.pow (b - y) 2 + Int.pow (c - z) 2

let box_of_string s =
  match String.split s ~on:',' with
  | [ a; b; c ] -> (Int.of_string a, Int.of_string b, Int.of_string c)
  | _ -> failwith "Invalid box string"

let all_connected (circuits : int Coord3dMap.t) =
  circuits |> Map.to_alist |> List.map ~f:snd
  |> List.all_equal ~equal:Int.equal
  |> Option.is_some

let connect (a, b) circuits visited n_circuits =
  if Set.mem visited a && Set.mem visited b then
    let id_a = Map.find_exn circuits a in
    let id_b = Map.find_exn circuits b in
    let circuits =
      Map.map ~f:(fun v -> if v = id_b then id_a else v) circuits
    in
    (circuits, visited, n_circuits)
  else if Set.mem visited a then
    let circuits = Map.set circuits ~key:b ~data:(Map.find_exn circuits a) in
    (circuits, Set.add visited b, n_circuits)
  else if Set.mem visited b then
    let circuits = Map.set circuits ~key:a ~data:(Map.find_exn circuits b) in
    (circuits, Set.add visited a, n_circuits)
  else
    let circuits =
      Map.set circuits ~key:a ~data:n_circuits
      |> Map.set ~key:b ~data:n_circuits
    in
    let visited = Set.add (Set.add visited a) b in
    (circuits, visited, n_circuits + 1)

let parse_input input =
  let boxes = input |> Input.lines |> List.map ~f:box_of_string in
  let sorted_connections =
    boxes |> List.pairs
    |> List.sort ~compare:(fun (a1, b1) (a2, b2) ->
           let d1 = distance a1 b1 in
           let d2 = distance a2 b2 in
           Int.compare d1 d2)
  in
  (boxes, sorted_connections)

let solve1 ?(stop = 1000) input =
  let _, sorted_connections = parse_input input in
  let rec aux connections circuits visited n_circuits =
    match connections with
    | [] -> circuits
    | (a, b) :: rest ->
        let circuits, visited, n_circuits =
          connect (a, b) circuits visited n_circuits
        in
        aux rest circuits visited n_circuits
  in
  aux (List.take sorted_connections stop) Coord3dMap.empty Coord3dSet.empty 0
  |> Map.to_alist
  |> List.map ~f:(fun (k, v) -> (v, k))
  |> Map.of_alist_multi (module Int)
  |> Map.to_alist |> List.map ~f:snd |> List.map ~f:List.length
  |> List.sort ~compare:(fun a b -> Int.compare b a)
  |> fun l -> List.take l 3 |> List.fold ~init:1 ~f:(fun acc x -> acc * x)

let solve2 input =
  let boxes, sorted_connections = parse_input input in

  let rec aux connections circuits visited n_circuits =
    match connections with
    | [] -> 0
    | (a, b) :: rest ->
        let circuits, visited, n_circuits =
          connect (a, b) circuits visited n_circuits
        in
        if Map.length circuits = List.length boxes && all_connected circuits
        then fst3 a * fst3 b
        else aux rest circuits visited n_circuits
  in
  aux sorted_connections Coord3dMap.empty Coord3dSet.empty 0
