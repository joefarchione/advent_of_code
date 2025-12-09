open! Core
include List

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

let range2 (n, m) =
  List.range 0 n
  |> List.map ~f:(fun i -> List.range 0 m |> List.map ~f:(fun j -> (i, j)))
  |> List.concat

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

let rec choose k list =
  match (k, list) with
  | 0, _ -> [ [] ]
  | _, [] -> []
  | k, head :: tail ->
      let with_head =
        let smaller_subsets = choose (k - 1) tail in
        List.map ~f:(fun subset -> head :: subset) smaller_subsets
      in
      let without_head = choose k tail in
      with_head @ without_head

let rec pairs list =
  match list with
  | [] -> []
  | head :: tail ->
      (* 1. Pair the head with every element in the tail *)
      let head_pairs = List.map ~f:(fun x -> (head, x)) tail in
      (* 2. Recursively find all combinations of size 2 from the tail *)
      let tail_combinations = pairs tail in
      (* 3. Combine the two sets of results *)
      head_pairs @ tail_combinations

let sort_desc compare lst = List.sort lst ~compare:(fun a b -> compare b a)
let take n l = List.take l n

let rec pairwise lst =
  match lst with
  | [] -> []
  | x :: y :: rest -> (x, y) :: pairwise rest
  | _ :: [] -> []

let rec pair_consecutive_elements lst =
  match lst with
  | [] -> []
  | x :: y :: rest -> (x, y) :: pair_consecutive_elements rest
  | _ :: [] ->
      [] (* Drops the last element if the list has an odd number of items *)
