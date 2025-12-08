open! Core

let memo f =
  let m = ref [] in
  fun x ->
    match List.Assoc.find !m ~equal:Poly.equal x with
    | Some v -> v
    | None ->
        let y = f x in
        m := (x, y) :: !m;
        y

let memo_rec f =
  let m = ref [] in
  let rec g x =
    match List.Assoc.find !m ~equal:Poly.equal x with
    | Some v -> v
    | None ->
        let y = f g x in
        m := (x, y) :: !m;
        y
  in
  g
