open! Core

type direction = Left | Right

let rotate direction degrees position = 
  match direction with 
  | Left -> (position - degrees) % 100  |> Int.abs
  | Right -> (position + degrees) % 100

let read filepath = 
  In_channel.read_lines filepath
  |> List.map ~f:(fun line ->  
    (
      (if String.equal (String.prefix line 1) "L" then Left else Right),
      String.drop_prefix line 1 |> Int.of_string
    )
  )

let num_of_zero_passes direction degrees position =
  let num_cycles = degrees / 100 in 
  if position = 0 then num_cycles
  else
    let change = degrees % 100 in 
    let next_position =  
      match direction with 
      | Left -> position - change
      | Right -> position + change
    in
    if next_position <= 0 then num_cycles + 1
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