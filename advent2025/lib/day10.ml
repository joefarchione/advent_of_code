open Aoc

module Light = struct
  type t = int [@@deriving sexp, equal, compare, hash]

  let ( + ) a b = (a + b) mod 2

  let of_char c =
    match c with
    | '#' -> 1
    | '.' -> 0
    | _ -> failwith (Printf.sprintf "Invalid character for status %c" c)
end

module Button = struct
  type t = Light.t list [@@deriving sexp, equal, compare, hash]

  let of_string (n : int) (str : string) : t =
    let indicators = String.split ~on:',' str |> List.map ~f:Int.of_string in
    List.init n ~f:(fun i ->
        if List.mem indicators i ~equal:Int.equal then 1 else 0)

  let ( + ) (a : t) (b : t) : t = List.map2_exn a b ~f:Light.( + )
  let create n = List.init n ~f:(fun _ -> 0)
end

module Joltages = struct
  type t = int list [@@deriving sexp, equal, compare, hash]

  let ( + ) (a : t) (b : t) : t = List.map2_exn a b ~f:( + )
  let equal a b = List.for_all2_exn a b ~f:Int.equal

  let max_trys a b =
    List.zip_exn a b
    |> List.filter ~f:(fun (_, x) -> x <> 0)
    |> List.map ~f:fst
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn

  let of_string (n : int) (str : string) : t =
    let indicators = String.split ~on:',' str |> List.map ~f:Int.of_string in
    List.init n ~f:(fun i ->
        if List.mem indicators i ~equal:Int.equal then 1 else 0)
end

let parse (line : string) =
  match line |> String.split ~on:' ' with
  | hd :: tl ->
      let indicator =
        String.to_list hd |> List.tl_exn |> List.drop_last_exn
        |> List.map ~f:Light.of_char
      in
      let buttons =
        List.drop_last_exn tl
        |> List.map
             ~f:
               (String.drop_first_and_last
               >> Button.of_string (List.length indicator))
      in
      let joltages =
        List.last_exn tl |> String.drop_first_and_last |> String.split ~on:','
        |> List.map ~f:Int.of_string
      in
      (indicator, buttons, joltages)
  | [] -> failwith "Invalid line format"

let read input = input |> String.split_lines |> List.map ~f:parse

let fewest_presses_to_match_indicator target (buttons : Button.t list) =
  let n = List.length buttons in
  List.init n ~f:(( + ) 1)
  |> List.fold_until ~init:Int.max_value
       ~f:(fun acc k ->
         match
           List.choose k buttons
           |> List.find ~f:(fun c ->
                  Button.equal (List.fold1 Button.( + ) c) target)
         with
         | Some _ -> Stop k
         | None -> Continue acc)
       ~finish:(fun acc -> acc)

let find_joltage_very_slowly target (buttons : Joltages.t list) =
  let max_weights = List.map buttons ~f:(fun b -> Joltages.max_trys target b) in
  max_weights
  |> List.map ~f:(fun mw -> List.init (mw + 1) ~f:Fn.id |> Sequence.of_list)
  |> Sequence.n_cartesian_product
  |> Sequence.filter ~f:(fun w ->
         List.map2_exn w buttons ~f:(fun weight button ->
             List.map button ~f:(fun x -> weight * x))
         |> List.fold1 Joltages.( + ) |> Joltages.equal target)
  |> Sequence.map ~f:(List.sum (module Int) ~f:Fn.id)
  |> Sequence.min_elt ~compare:Int.compare
  |> Option.value_exn

let fewest_presses_to_match_joltages (target : Joltages.t)
    (buttons : Button.t list) =
  let button_coeffs =
    List.mapi buttons ~f:(fun j b ->
        let ub = Joltages.max_trys target b |> Float.of_int in
        Lp.var ~integer:true ~lb:0.0 ~ub (sprintf "x_%d" j))
  in

  let target = target |> List.map ~f:Float.of_int in
  let buttons = List.map buttons ~f:(List.map ~f:Float.of_int) in

  let object_minimize_coefficient_sum =
    List.fold1 Lp.( ++ ) button_coeffs |> Lp.minimize
  in

  let constraints_sum_matches_joltage =
    List.mapi target ~f:(fun light_index light_value ->
        let b_eq_val = light_value in

        let lhs_expr =
          List.fold2_exn buttons button_coeffs ~init:Lp.zero
            ~f:(fun acc jolts coeff ->
              Lp.(acc ++ (coeff *~ Lp.c (List.nth_exn jolts light_index))))
        in
        Lp.eq lhs_expr (Lp.c b_eq_val))
  in

  let problem =
    Lp.make object_minimize_coefficient_sum constraints_sum_matches_joltage
  in

  match Lp_glpk.solve problem ~term_output:false with
  | Ok (_, xs) ->
      Lp.PMap.to_list xs |> List.map ~f:snd
      |> List.sum (module Int) ~f:Float.to_int
  | Error msg ->
      printf "GLPK Error: %s\n" msg;
      0

let solve1 input =
  read input
  |> List.map ~f:(fun (target, buttons, _) ->
         fewest_presses_to_match_indicator target buttons)
  |> List.sum (module Int) ~f:Fn.id

let solve2 input =
  read input
  |> List.map ~f:(fun (_, buttons, joltages) ->
         fewest_presses_to_match_joltages joltages buttons)
  |> List.sum (module Int) ~f:Fn.id
