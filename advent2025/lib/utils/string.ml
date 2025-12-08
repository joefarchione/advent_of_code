include Core.String

let chunk_string (s : t) (n : int) =
  let len = length s in
  let rec aux (i : int) (acc : t list) =
    if Int.equal (Int.compare i len) 1 then List.rev acc
    else
      let chunk_len = Int.min n (len - i) in
      let chunk = sub s ~pos:i ~len:chunk_len in
      aux (i + n) (chunk :: acc)
  in
  aux 0 []

let split_on_char on = split_on_chars ~on:[ on ]
