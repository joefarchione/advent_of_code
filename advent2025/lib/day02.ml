open! Core
open Utils

type product_id = char list

let product_id_range a b = 
  range_inclusive (Int.of_string a) (Int.of_string b)
  |> Seq.map Int.to_string

let has_even_number_of_digits id = String.length id mod 2 = 0

let read filepath = 
  In_channel.read_lines filepath
  |> List.hd_exn
  |> String.split_on_chars ~on:[',']
  |> List.map ~f:(fun range_str ->
      match String.split_on_chars ~on:['-'] range_str with
      | [a; b] -> (a, b)
      | _ -> failwith "Invalid range format"
  )

let is_repeated_n n s = 
  let chunks = chunk_string s n in
  match chunks with
  | first_chunk :: rest_chunks ->
      List.for_all rest_chunks ~f:(fun chunk -> String.equal chunk first_chunk)
  | [] -> false

let is_repeated_twice s = 
  let midpoint = String.length s / 2 in
  Seq.init midpoint (fun i -> (String.get s i, String.get s (midpoint + i)))
  |> Seq.for_all (unpack2 Char.equal)

let is_repeated s = 
  Seq.init ((String.length s) / 2) (fun i -> i)
  |> Seq.find (fun i -> is_repeated_n (i + 1) s)
  |> Option.is_some

let solve1 filepath =
  read filepath
  |> List.map ~f:(fun (a, b) ->
      product_id_range a b
      |> Seq.filter has_even_number_of_digits
      |> Seq.filter is_repeated_twice
      |> Seq.map Int.of_string
      |> Seq.fold_left (+) 0
  )
  |> List.fold ~init:0 ~f:(+)
  |> printf "%d"
      
let solve2 filepath =
  read filepath
  |> List.map ~f:(fun (a, b) ->
      product_id_range a b
      |> Seq.filter is_repeated
      |> Seq.map Int.of_string
      |> Seq.fold_left (+) 0
  )
  |> List.fold ~init:0 ~f:(+)
  |> printf "%d"
      




  
