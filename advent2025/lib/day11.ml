open Aoc

let read input =
  input |> String.split_lines
  |> List.map ~f:(fun l ->
         let open Re in
         let pat = alpha |> rep1 |> compile in
         let strings = Re.all pat l |> List.map ~f:(fun g -> Group.get g 0) in
         (List.hd_exn strings, List.tl_exn strings))
  |> Map.of_alist_exn (module String)

module Key = struct
  type t = string * string * int [@@deriving sexp, hash, compare]
end

let paths_a_to_b ?(req_nodes = Set.empty (module String)) a b cnxns =
  let inc_req_count count node =
    if Set.mem req_nodes node || Set.is_empty req_nodes then count + 1
    else count
  in
  let aux =
    Memo.memo_rec
      (module Key)
      (fun aux (a, b, req_count) ->
        if String.equal a b then
          if req_count >= Set.length req_nodes then 1 else 0
        else
          let required = inc_req_count req_count a in
          Map.find_exn cnxns a
          |> List.fold ~init:0 ~f:(fun acc v -> acc + aux (v, b, required)))
  in
  aux (a, b, 0)

let solve1 input = read input |> paths_a_to_b "you" "out"

let solve2 input =
  read input
  |> paths_a_to_b
       ~req_nodes:(Set.of_list (module String) [ "fft"; "dac" ])
       "svr" "out"
