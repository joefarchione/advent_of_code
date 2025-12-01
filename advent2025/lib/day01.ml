open! Core

type direction = Left | Right

let rotate direction degrees position = 
  match direction with 
  | Left -> (position - degrees) % 100  |> Int.abs
  | Right -> (position + degrees) % 100

let read filepath = 
  In_channel.read_lines filepath
  |> List.map ~f:(fun line ->  
    match String.to_list line with
    | 'R'::tl -> (Right, Int.of_string (String.of_list tl))
    | 'L'::tl -> (Left, Int.of_string (String.of_list tl))
    | _ -> failwith "Invalid input"
  )

let num_of_zero_passes direction degrees position =
  let num_cycles = degrees / 100 in 
  let change = degrees % 100 in 
  let next_position =  
    match direction with 
    | Left -> position - change
    | Right -> position + change
  in
  if position = 0 then num_cycles
  else if next_position <= 0 then num_cycles + 1
  else if next_position >= 100 then num_cycles + 1
  else num_cycles


let solve1 filepath = 
  read filepath
  |> List.fold ~init:(50, 0) ~f:(fun (position, count) (direction, degrees) -> 
      let new_position = rotate direction degrees position in
      (new_position, count + (if new_position = 0 then 1 else 0))
  )
  |> snd
  |> Printf.printf "%d"

let solve2 filepath = 
  read filepath
  |> List.fold ~init:(50, 0) ~f:(fun (position, count) (direction, degrees) -> 
      let new_position = rotate direction degrees position in
      (new_position, count + num_of_zero_passes direction degrees position)
  )
  |> snd
  |> Printf.printf "%d"