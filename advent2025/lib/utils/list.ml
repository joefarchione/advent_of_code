open! Core
include List

let range = fun n -> List.init n ~f:Fn.id

let split_on ~on lst =
  List.split_while lst ~f:(fun x -> not (on x)) |> fun (before, after) ->
  (before, List.drop after 1)

let cumsum mod_ lst = List.sum mod_ lst ~f:Fn.id

let zip_longest ~default l1 l2 =
  let rec aux l1 l2 =
    match (l1, l2) with
    | [], [] -> []
    | [], y :: ys -> (default, y) :: aux [] ys
    | x :: xs, [] -> (x, default) :: aux xs []
    | x :: xs, y :: ys -> (x, y) :: aux xs ys
  in
  aux l1 l2

let zip_n lsts =
  let rec aux lsts =
    if List.for_all lsts ~f:List.is_empty then []
    else
      let heads = List.map lsts ~f:List.hd_exn in
      let tails = List.map lsts ~f:List.tl_exn in
      heads :: aux tails
  in
  aux lsts

let hd_or ~default lst = match lst with [] -> default | x :: _ -> x
let tl_or ~default lst = match lst with [] -> default | _ :: xs -> xs

let zip_n_longest ~default lsts =
  let rec aux lsts =
    if List.for_all lsts ~f:List.is_empty then []
    else
      let heads = List.map lsts ~f:(hd_or ~default) in
      let tails = List.map lsts ~f:(tl_or ~default:[]) in
      heads :: aux tails
  in
  aux lsts

let hd_tl_exn lst =
  match lst with
  | [] -> failwith "Empty list has no head and tail"
  | x :: xs -> (x, xs)

let split pred l =
  let rec aux acc current = function
    | [] -> List.rev (current :: acc)
    | x :: xs ->
        if pred x then aux (current :: acc) [] xs else aux acc (x :: current) xs
  in
  aux [] [] l |> List.rev

let max_elt_i lst ~compare =
  let rec aux lst cmax cindex index =
    match lst with
    | [] -> (cmax, cindex)
    | hd :: tl ->
        if compare hd cmax > 0 then aux tl hd index (index + 1)
        else aux tl cmax cindex (index + 1)
  in
  match lst with [] -> None | hd :: tl -> Some (aux tl hd 0 1)

let rec remove_last l =
  match l with [] -> [] | [ _ ] -> [] | hd :: tl -> hd :: remove_last tl
