open Core
type robot = {position: Utils.Coords.t; velocity: Utils.Coords.t} 
let show t = Printf.sprintf "{position=%s; velocity=%s}" (Utils.Coords.show t.position) (Utils.Coords.show t.velocity)

let move dim robot = 
  let next_position = Utils.Coords.{
    x = Utils.modulo (robot.position.x + robot.velocity.x) dim.y;
    y = Utils.modulo (robot.position.y + robot.velocity.y) dim.x;
  } in
  {robot with position = next_position}

let read filepath = 
  let re_str = Re.(seq [opt (str "-"); rep1 digit;] |> compile) in 
  In_channel.read_lines filepath
  |> List.map ~f:(fun line ->
    match Re.matches re_str line with 
    | [x; y; v1; v2;] ->  {
      position = Utils.Coords.{x = int_of_string x; y = int_of_string y;};
      velocity = Utils.Coords.{x = int_of_string v1; y = int_of_string v2;};
    }
    | _ -> failwith "incorrect input"
  )

let rec move_n n dim robot = 
  match n with 
  | 0 -> robot
  | _ -> move_n (n-1) dim (move dim robot)

type quadrants = {a:int; b:int; c:int; d: int}

let calculate_safety_factor (dim: Utils.Coords.t) (robots: robot list) = 
  let mx = if ((dim.y-1) mod 2) = 0 then ((dim.y-1) / 2) else (((dim.y-1) / 2) + 1) in 
  let my = if ((dim.x-1) mod 2) = 0 then ((dim.x-1) / 2) else (((dim.x-1) / 2) + 1) in 

  let count = List.fold robots ~init:{a=0;b=0;c=0;d=0;} ~f:(fun q robot ->
    if (robot.position.x < mx) && (robot.position.y < my) then {q with a = q.a+1}
    else if (robot.position.x < mx) && (robot.position.y > my) then {q with b = q.b+1}
    else if (robot.position.x > mx) && (robot.position.y > my) then {q with c = q.c+1}
    else if (robot.position.x > mx) && (robot.position.y < my) then {q with d = q.d+1}
    else q
  ) in 
  count.a * count.b * count.c * count.d

module CoordsSet = Set.Make(Utils.Coords)
let not_unique_coords =
  let rec loop (ss:CoordsSet.t) (coords: Utils.Coords.t list)  = 
  match coords with
  | hd :: tl -> Set.mem ss hd || loop (Set.add ss hd) tl
  | [] -> false
  in 
  loop CoordsSet.empty

let move_robots (dim: Utils.Coords.t) robots = 
  List.map robots ~f:(move_n 1 dim)

let iter_until_unique_coords dim = 
  let rec aux iters robots = 
    if not_unique_coords (List.map robots ~f:(fun p -> p.position)) then aux (Int.succ iters) (move_robots dim robots)
    else iters 
  in  
  aux 0

let solve_p1 (dim: Utils.Coords.t) robots = 
  List.map robots ~f:(move_n 100 dim)
  |> calculate_safety_factor dim 
  |> Printf.printf "%d"

let solve_p2 (dim: Utils.Coords.t) robots = 
  iter_until_unique_coords dim robots
  |> Printf.printf "%d"





