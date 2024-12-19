open Core
open Re

module FrequencyMap = Map.Make(Int)

let read filepath = 
  In_channel.read_lines filepath
    |> List.map ~f:(fun s -> Str.split  (Str.regexp "[ \n\r\x0c\t]+") s)
    |> List.map ~f:(fun s_s -> 
      match s_s with 
      | one::two::_ -> ((Int.of_string one), (Int.of_string two))
      | _ -> failwith "not enough numbers"
    )

let sorted_unzip lst = 
  lst
  |> List.unzip
  |> (fun (left, right) -> (
        (List.sort ~compare:Int.compare left) ,
        (List.sort ~compare:Int.compare right) ))

let to_frequency_map lst = 
  List.fold_left lst ~init:FrequencyMap.empty ~f:(fun acc l ->
    match Map.find acc l with 
    | Some (v) -> Map.set acc ~key:l ~data:(v+1)
    | None -> Map.set acc ~key:l ~data:1
  )

let calculate_distance lst  = 
    lst 
    |> (fun (left, right) -> 
        List.fold2_exn left right
          ~init:0 
          ~f:(fun acc a b -> Int.abs (a-b) + acc)
      ) 

let calculate_simularity left right = 
  let freq_map = to_frequency_map right in 
  List.fold_left left ~init:0 ~f:(
    fun acc l -> 
      match Map.find freq_map l with 
      | Some (v) -> acc + (l*v)
      | None -> acc
  )

let solve_p1 filepath = 
  filepath 
  |> read
  |> sorted_unzip 
  |> calculate_distance
  |> (Printf.printf "%d") 


let solve_p2 filepath = 
  filepath 
  |> read 
  |> List.unzip 
  |> (fun (left, right) -> calculate_simularity left right )
  |> (Printf.printf "%d")

