open Core

module Position = struct

  type t = {x:int; y:int;} [@@deriving sexp, equal, compare]

  let up position = {position with x = position.x-1 }
  let down position = {position with x = position.x +1 }
  let left position = {position with y = position.y -1 }
  let right position = {position with y = position.y+1 }

  let in_dimensions (dimx, dimy) position = 
    (position.x >= 0)
    && (position.x < dimx)
    && (position.y >= 0)
    && (position.y < dimy)

  let all_positions (dimx, dimy) = 
    List.fold Utils.(0--(dimx-1))
      ~init:[]
      ~f:(fun acc x -> List.fold ~init:acc Utils.(0--(dimy-1)) ~f:(fun acc y -> {x;y;}::acc))
end

module PositionSet = Set.Make(Position)

module Border = struct
  type t = Above | Below | Right | Left [@@deriving sexp, equal, compare]
  let to_string b =
    match b with 
    | Above -> "Above"
    | Below -> "Below"
    | Right -> "Right"
    | Left -> "Left"
end

module BorderSet = struct 
  module M = Set.Make(Border)
  include M

  type t = M.t

  let show (t:t) = 
    List.map (Set.elements t) ~f:Border.to_string
    |> String.concat ~sep:","
end

module Space = struct 
  type t = {position:Position.t; value: char; borders: BorderSet.t}  [@@deriving sexp, equal, compare]

  let move direction = 
    let open Position in 
    let open Border in 
    match direction with 
    | Above -> up
    | Below -> down
    | Right -> right
    | Left -> left

  let show t = 
    Printf.sprintf "%c at (%d,%d) with borders %s" t.value t.position.x t.position.y (BorderSet.show t.borders)
  end

module SpaceSet = Set.Make(Space)

module Region = struct
  type t = {
    area: Space.t array array;
    current: Space.t;
    dim: int*int;
  }

  let move_space region direction (space: Space.t) = 
    let next_position = (Space.move direction) space.position in
    if Position.in_dimensions region.dim next_position then 
      Some region.area.(next_position.x).(next_position.y)
    else 
      None

  let get_neighbors region space : (Space.t list)= 
    [
      move_space region Border.Above space;
      move_space region Border.Below space;
      move_space region Border.Right space;
      move_space region Border.Left space;
    
    ]
    |> Utils.filter_some

  let get_neighbors_same_plant region space = 
    get_neighbors region space
    |> List.filter ~f:(fun s-> Char.equal space.value s.value)

  let get_border_direction region space direction  = 
    match move_space region direction space with
    | Some (s) when Char.equal s.value space.value -> None
    | _ -> Some direction

  let get_borders_of_space region space = 
    let aux = get_border_direction region space in 
    [aux Above; aux Below; aux Left; aux Right;]
    |> Utils.filter_some
    |> BorderSet.of_list

  let get_all_borders region = 
    let area = region.area 
        |> Array.map ~f:(fun r -> Array.map r ~f:(fun s -> {s with borders = get_borders_of_space region s})) in 
    {region with area=area; current = area.(0).(0)}

  let walk_garden region space unvisited  = 
    let rec aux region (garden: SpaceSet.t) (space: Space.t) unvisited = 
      let unvisited = Set.remove unvisited space.position in 
      let garden = Set.add garden space in 
      match get_neighbors_same_plant region space |> List.filter ~f:(fun s ->  not (Set.mem garden s) && (Set.mem unvisited s.position)) with 
      | [] -> (garden, unvisited)
      | plants -> List.fold plants ~init:(garden, unvisited) ~f:(fun (acc, unvisited) p -> 
        let (acc, unvisited) = aux region acc p unvisited in 
        (acc, unvisited)
      )
    in
    aux region SpaceSet.empty space unvisited

  type gardens = SpaceSet.t list

  let walk_gardens region = 
    let rec aux region gardens unvisited = 
      let (garden, unvisited) = walk_garden region region.current unvisited in 
      let gardens = garden::gardens in
      if Set.is_empty unvisited then 
        gardens
      else
          let next = Set.min_elt_exn unvisited in
          let region = {region with current = region.area.(next.x).(next.y)} in 
          aux region gardens unvisited
    in 
    let unvisited = PositionSet.of_list (Position.all_positions region.dim) in 
    aux region [] unvisited

  let calc_area (garden: SpaceSet.t) = 
    let (num_plants, num_borders) = Set.fold ~init:(0,0) garden ~f:(fun (num_plants, num_borders) space -> 
      (num_plants + 1, num_borders + (Set.length space.borders))
    ) in 
    num_plants * num_borders

  let calc_num_corners region (space: Space.t) (garden: SpaceSet.t) = 
    let aux a b = 
      let up = move_space region a space in 
      let right = move_space region b space in 
      match (up, right) with 
      | (None, None) -> 1
      | (Some up, Some right) when (not Set.(mem garden up)) && (not (Set.mem garden right)) -> 1
      | (None, Some (left)) when not (Set.mem garden left) -> 1
      | (Some (right), None) when not (Set.mem garden right) -> 1
      | (None, Some _) | (Some _, None) -> 0
      | (Some up, Some right) when (Set.mem garden up) && (Set.mem garden right) -> (
        let diag = move_space region b up in 
        match diag with 
        | None -> 0
        | Some (d) when Set.mem garden d -> 0
        | Some (_) -> 1
      )
      | _ -> 0
    in 
    let num_corners =
      aux Border.Above Border.Right
      + aux Border.Above Border.Left
      + aux Border.Below Border.Right
      + aux Border.Below Border.Left in 

    num_corners

  let calc_discount_price region (garden: SpaceSet.t) = 
    let (num_plants, num_corners) = Set.fold ~init:(0,0) garden ~f:(fun (num_plants, num_corners) space -> 
      (num_plants + 1, num_corners + (calc_num_corners region space garden)))
    in 
    num_plants * num_corners


  let calc_area_of_gardens (gardens: gardens) = 
    List.map gardens ~f:calc_area 
    |> List.fold ~init:0 ~f:(+)

  let calc_discount_prices region (gardens: gardens) = 
    List.map gardens ~f:(fun g -> calc_discount_price region g)
    |> List.fold ~init:0 ~f:(+)

end

let read filepath  = 
  let area = In_channel.read_lines filepath 
    |> List.map ~f:String.to_list 
    |> List.mapi ~f:(fun x row -> List.mapi row ~f:(fun y c ->  (Space.{position={x;y;};value=c;borders=BorderSet.empty})))
    |> (fun l_of_l -> Array.of_list (List.map ~f:Array.of_list l_of_l))
  in
  let dim = (Array.length area,Array.length (area.(0))) in 
  Region.{area; current=area.(0).(0); dim;}

let solve_p1 (region: Region.t) = 
  region
  |> Region.get_all_borders
  |> Region.walk_gardens 
  |> Region.calc_area_of_gardens 
  |> Printf.printf "%d"

let solve_p2 (region: Region.t) = 
  let region = region |> Region.get_all_borders in 
  let gardens = Region.walk_gardens region in 
  Region.calc_discount_prices region gardens
  |> Printf.printf "%d"



