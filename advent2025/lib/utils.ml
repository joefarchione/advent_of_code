open! Core

let chunk_string s n =
  let len = String.length s in
  let rec aux i acc =
    if i >= len then
      List.rev acc
    else
      let chunk_len = min n (len - i) in (* Calculate remaining length for the last chunk *)
      let chunk = String.sub s ~pos:i ~len:chunk_len in
      aux (i + n) (chunk :: acc)
  in
  aux 0 []

let unpack2 f (a, b) = f a b

let range_inclusive a b = 
  Seq.init (b - a + 1) (fun i -> a + i)

let range_exclusive a b = 
  Seq.init (b - a) (fun i -> a + i)