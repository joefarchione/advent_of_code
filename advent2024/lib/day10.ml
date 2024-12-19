open Core

module Hiker = struct
  type t = {x:int; y:int;} [@@deriving sexp, compare, equal]

  let left t = {t with y = t.y-1}
  let right t = {t with y = t.y+1}
  let up t = {t with x = t.x-1}
  let down t = {t with x = t.x+1}

  let next hiker = [(left hiker); (right hiker); (up hiker); (down hiker)]

end

module AreaMap = struct
  module M = Map.Make(Hiker) 
  include M

  type t = int M.t

  let from_tuples c_pos = 
    let rec aux c_pos map = 
      match c_pos with 
      | (hiker, value)::tl -> (
        let map = Map.update map hiker ~f:(fun v_opt -> 
            match v_opt with
            | Some (_) -> value
            | None ->  value
        )
        in
        aux tl map
      )
      | [] -> map
    in
    let rules = M.empty in 
    aux c_pos rules

  let find_trailheads (t:t) = 
    Map.filter t ~f:(fun v -> Int.equal v 0)
    |> Map.fold ~f:(fun ~key:key ~data:_ acc -> key :: acc) ~init:[]

end

module SummitSet = Set.Make(Hiker)

module Area = struct
  type t = AreaMap.t

  let is_gradual_change map p1 p2 = 
    let v1 = Map.find map p1 in 
    let v2 = Map.find map p2 in 
    match (v1, v2) with 
    | (Some (a), Some (b)) -> Int.equal (b - a) 1
    | _ -> false

  let at_summit t hiker = 
    match Map.find t hiker with 
    | Some (v) -> Int.equal v 9
    | None -> false

  let next_positions t (hiker: Hiker.t) = 
    let open Hiker in
    next hiker
    |> List.filter ~f:(fun new_pos -> is_gradual_change t hiker new_pos)

  let find_score t hiker = 
    let rec find_score' (t: t) (hiker:Hiker.t) (set: SummitSet.t) = 
      let possible_positions = next_positions t hiker in 

      List.fold possible_positions ~init:set ~f:(fun acc next_hiker -> 
        match next_hiker with 
        | summit when (at_summit t next_hiker) -> (Set.add acc summit)
        | _ -> (find_score' t next_hiker acc)
      )
    in

    find_score' t hiker SummitSet.empty
    |> Set.length

  let find_rating t hiker = 
    let rec find_rating' (t: t) (hiker:Hiker.t) acc = 
      let possible_positions = next_positions t hiker in 

      List.fold possible_positions ~init:acc ~f:(fun acc next_hiker -> 
        match next_hiker with 
        | _ when (at_summit t next_hiker) -> acc + 1
        | _ -> (find_rating' t next_hiker acc)
      )
    in

    find_rating' t hiker 0
end

let read filepath = 
  In_channel.read_lines filepath
  |> List.mapi ~f:(
    fun ii line -> List.mapi (String.to_list line) ~f:(fun jj c -> (Hiker.{x=ii;y=jj}, (Char.get_digit_exn c))))
  |> List.concat
  |> AreaMap.from_tuples

let solve_p1 (map: AreaMap.t) = 
  let trailheads = AreaMap.find_trailheads map in 
  trailheads 
  |> List.fold ~init:0 ~f:(fun acc hiker -> acc + (Area.find_score map hiker))
  |> (Printf.printf "%d")

let solve_p2 (map: AreaMap.t) = 
  let trailheads = AreaMap.find_trailheads map in 
  trailheads 
  |> List.fold ~init:0 ~f:(fun acc hiker -> acc + (Area.find_rating map hiker))
  |> (Printf.printf "%d")





