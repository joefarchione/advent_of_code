open Aoc

let distance (a, b, c) (x, y, z) =
  Int.pow (a - x) 2 + Int.pow (b - y) 2 + Int.pow (c - z) 2

let all_connected (circuits : int Coord3.Map.t) =
  circuits |> Map.to_alist |> List.map ~f:snd
  |> List.all_equal ~equal:Int.equal
  |> Option.is_some

let connect (a, b) circuits n_circuits =
  match (Map.find circuits a, Map.find circuits b) with
  | Some id_a, Some id_b ->
      let circuits =
        Map.map ~f:(fun v -> if v = id_b then id_a else v) circuits
      in
      (circuits, n_circuits)
  | Some id_a, None ->
      let circuits = Map.set circuits ~key:b ~data:id_a in
      (circuits, n_circuits)
  | None, Some id_b ->
      let circuits = Map.set circuits ~key:a ~data:id_b in
      (circuits, n_circuits)
  | None, None ->
      let circuits =
        Map.set circuits ~key:a ~data:n_circuits
        |> Map.set ~key:b ~data:n_circuits
      in
      (circuits, n_circuits + 1)

let parse_input input =
  let boxes = input |> Input.lines |> List.map ~f:Coord3.of_string in
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
  let rec aux connections circuits n_circuits =
    match connections with
    | [] -> circuits
    | (a, b) :: rest ->
        let circuits, n_circuits = connect (a, b) circuits n_circuits in
        aux rest circuits n_circuits
  in
  aux (List.take stop sorted_connections) Coord3.Map.empty 0
  |> Map.to_alist
  |> List.map ~f:(fun (k, v) -> (v, k))
  |> Map.of_alist_multi (module Int)
  |> Map.to_alist |> List.map ~f:snd |> List.map ~f:List.length
  |> List.sort_desc Int.compare |> List.take 3
  |> List.fold ~init:1 ~f:(fun acc x -> acc * x)

let solve2 input =
  let boxes, sorted_connections = parse_input input in

  let rec aux connections circuits n_circuits =
    match connections with
    | [] -> 0
    | (a, b) :: rest ->
        let circuits, n_circuits = connect (a, b) circuits n_circuits in
        if Map.length circuits = List.length boxes && all_connected circuits
        then fst3 a * fst3 b
        else aux rest circuits n_circuits
  in
  aux sorted_connections Coord3.Map.empty 0
