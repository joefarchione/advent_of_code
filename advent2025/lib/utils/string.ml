module T = struct
  let chunk_string (s : string) (n : int) =
    let len = Core.String.length s in
    let rec aux (i : int) (acc : string list) =
      if Int.equal (Int.compare i len) 1 then List.rev acc
      else
        let chunk_len = Int.min n (len - i) in
        let chunk = Core.String.sub s ~pos:i ~len:chunk_len in
        aux (i + n) (chunk :: acc)
    in
    aux 0 []

  let split_on_char on = Core.String.split_on_chars ~on:[ on ]

  let drop_first_and_last s =
    let len = Core.String.length s in
    if len < 3 then ""
      (* Return an empty string if the input is too short to drop two chars *)
    else Core.String.sub s ~pos:1 ~len:(len - 2)
end

include Core.String
include T
