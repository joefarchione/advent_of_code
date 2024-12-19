open Core

module Position = struct
  type t = {x:int; y:int;} [@@deriving sexp, compare, equal]

  let valid dim t = 
    (t.x < dim.x) && (t.x >=0) 
    && (t.y < dim.y) && (t.y >=0) 

  let (-) p1 p2 = {x=p1.x-p2.x; y = p1.y-p2.y}
  let (+) p1 p2 = {x=p1.x+p2.x; y = p1.y+p2.y}
  let (~-) p = {x= ~-(p.x); y = ~-(p.y)}

end

module PositionSet = Set.Make(Position)

module PositionMapping = struct
  module M = Map.Make(Char) 
  include M

  let from_tuples pairs = 
    let rec from_tuples' pairs map = 
      match pairs with 
      | (Some (c, p))::tl -> (
        let map = Map.update map c ~f:(fun v_opt -> 
            match v_opt with
            | Some (v) -> p :: v
            | None -> [p]
        )
        in
        from_tuples' tl map
      )
      | None::tl -> from_tuples' tl map
      | [] -> map
    in
    let rules = M.empty in 
    from_tuples' pairs rules
end

(* module AntinodeMap = struct

  type t = {mapping: (Position.t list) PositionMapping.t; dimension: Position.t; valid: Position.t -> bool; all_positions: PositionSet.t;}

  let from_file filepath = 
    let lines = In_channel.read_lines filepath
    in
    let dimension = 
      let x = List.length lines in 
      let y = match List.nth lines 0 with | Some (l) -> String.length l | None -> failwith "empty line" in 
      Position.{x;y;}
    in
    let positions = 
      lines 
      |> List.mapi ~f:(fun ii line -> 
          String.to_list line 
          |> List.mapi ~f:(fun jj c -> match c with | '.' -> None | _ -> Some (c, Position.{x=ii;y=jj;})))
      |> List.concat
    in
    let all_positions = 
      List.fold_left positions ~init:PositionSet.empty
        ~f:(fun set pos_op -> 
            match pos_op with 
            | Some (_, p) -> Set.add set p
            | None -> set)
    in
    let mapping = positions |> PositionMapping.from_tuples in 
    {mapping; dimension; valid = Position.valid dimension; all_positions;}

  let find_antinode_positions (t:t) (a:Position.t) (b:Position.t) = 
    let add_if_valid t set pos = 
      if t.valid pos && (not (Set.mem t.all_positions pos)) then 
          Set.add set pos
      else
        set
    in

    let rec aux t a b ant_positions = 
      let delta = Position.(a - b) in 
      let ant_positions = Position.(a + delta) |> (add_if_valid t ant_positions) in 
      let ant_positions = Position.(b - delta) |> (add_if_valid t ant_positions) in 
      ant_positions


end
 *)