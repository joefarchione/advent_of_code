open Core

module Position = struct 

  type t = {x:int; y:int} [@@deriving sexp, equal, compare]

  let down t = {t with x=t.x+1}
  let up t = {t with x=t.x-1}
  let right t = {t with y=t.y+1}
  let left t = {t with y=t.y-1}

  type direction = Up | Down | Left | Right

  let direction_of_char = function
  | '<' -> Left
  | '>' -> Right
  | 'v' -> Down
  | '^' -> Up
  | _ -> failwith "incorrect character"

  let move direction t = 
    match direction with 
    | Up -> up t
    | Down -> down t
    | Left -> left t
    | Right -> right t

end

module Object = struct 
  type t = Empty | Box | Robot | Wall [@@deriving sexp, equal, compare]

  let of_char = function
  | 'O' -> Box
  | '#' -> Wall
  | '.' -> Empty
  | '@' -> Robot
  | _ -> failwith "incorrect object string"

  let to_char = function
  | Box -> 'O'
  | Wall -> '#'
  | Empty -> '.'
  | Robot -> '@'

end

module Grid = struct 
  module M = Map.Make(Position) 
  include M

  type space = {position: Position.t; value: Object.t} 
  type t = space M.t

  let print t = 
    let a = Array.make_matrix ~dimx:8 ~dimy:8 '.' in 
    Map.iter t ~f:(fun v -> a.(v.position.x).(v.position.y) <- (Object.to_char v.value));

    let strings = Array.map a ~f:(fun row -> Array.fold row ~init:"" ~f:(fun acc r -> acc ^ (String.make 1 r))) in 

    Array.iter strings ~f:print_endline;;

  let next t direction space = 
    let next_position = Position.move direction space.position in 
    Map.find_exn t next_position

  let change_value grid position new_value = 
    Map.update grid position 
      ~f:(function | Some (v) -> {v with value = new_value} | None -> failwith "incorrect position")

  let rec move_boxes t direction space = 
    let next_space = next t direction space in 
    match next_space.value with 
    | Empty -> Some (change_value t next_space.position Object.Box)
    | Box -> move_boxes t direction next_space
    | Wall -> None
    | _ -> failwith "shouldn't find a robot again"

  let move grid direction robot  = 
    let next_space = next grid direction robot in 
    match next_space with 
    | {value = Object.Empty; _} -> (
      let grid = change_value grid robot.position Object.Empty in 
      let grid = change_value grid next_space.position Object.Robot in 
      let robot = {robot with position = next_space.position} in 
      (grid, robot)
    )
    | {value = Object.Box; _} -> (
      let grid_opt = move_boxes grid direction next_space in 
      match grid_opt with 
      | Some (grid) -> (
        let grid = change_value grid next_space.position Object.Robot in 
        let grid = change_value grid robot.position Object.Empty in 
        let robot = {robot with position = next_space.position} in 
        (grid, robot)
      )
      | None -> (grid, robot)
      
    )
    | {value = Object.Wall; _} -> (grid, robot)
    | _ -> failwith (Printf.sprintf "%d %d %s" next_space.position.x next_space.position.y (Sexp.to_string (Object.sexp_of_t next_space.value)))


end

type inputs = {grid: Grid.t; directions: Position.direction list; robot: Grid.space}

let read filepath = 
  In_channel.read_lines filepath
  |> Utils.split_on_newline
  |> (function
    | [grid; moves] -> (
      let mapping = grid
        |> List.map ~f:String.to_list
        |> List.foldi ~init:Grid.empty ~f:(fun x acc line -> 
          List.foldi line ~init:acc ~f:(fun y acc c -> 
            Map.add_exn 
              acc 
              ~key:Position.{x;y;} 
              ~data:Grid.{position=Position.{x;y;}; value = Object.of_char c})
        )
        in 

      let directions = moves 
        |> List.map ~f:String.to_list
        |> List.concat
        |> List.map ~f:(Position.direction_of_char)
      in

      let ((x,y), _) = grid
        |> List.map ~f:String.to_list
        |> List.foldi ~init:[] ~f:(fun x acc line -> List.foldi line ~init:acc ~f:(fun y acc c -> ((x,y), c)::acc))
        |> List.filter ~f:(fun ((_,_), c) -> Char.equal c '@')
        |> List.hd_exn
      in
      let robot = Grid.{position=Position.{x;y;}; value=Object.Robot} in 

      {grid=mapping; directions=directions; robot = robot}
    )
    | _ -> failwith "incorrect input"
  )

let calculate_score (position: Position.t) = 100 * position.x + position.y

let solve_p1 input = 
  let (grid, _) = List.fold input.directions ~init:(input.grid, input.robot) 
    ~f:(fun (grid, robot) direction -> 
      let (grid, robot) = Grid.move grid direction robot in 
      (grid, robot)) in 

  Map.filter grid ~f:(fun v -> Poly.equal v.value Object.Box)
  |> Map.fold ~init:0 ~f:(fun ~key:p ~data:_ acc -> (calculate_score p) + acc)
  |> Printf.printf "%d"