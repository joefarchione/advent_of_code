open! Core

let chunk_string s n =
  let len = String.length s in
  let rec aux i acc =
    if i >= len then List.rev acc
    else
      let chunk_len = min n (len - i) in
      (* Calculate remaining length for the last chunk *)
      let chunk = String.sub s ~pos:i ~len:chunk_len in
      aux (i + n) (chunk :: acc)
  in
  aux 0 []

let unpack2 f (a, b) = f a b
let range_inclusive a b = Seq.init (b - a + 1) (fun i -> a + i)
let range_exclusive a b = Seq.init (b - a) (fun i -> a + i)

let rec remove_last l =
  match l with [] -> [] | [ _ ] -> [] | hd :: tl -> hd :: remove_last tl

module List = struct
  include List

  let max_elt_i lst ~compare =
    let rec aux lst cmax cindex index =
      match lst with
      | [] -> (cmax, cindex)
      | hd :: tl ->
          if compare hd cmax > 0 then aux tl hd index (index + 1)
          else aux tl cmax cindex (index + 1)
    in
    match lst with [] -> None | hd :: tl -> Some (aux tl hd 0 1)
end
