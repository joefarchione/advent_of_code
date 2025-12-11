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

let memo_rec module_ f =
  let table = Hashtbl.create module_ in
  let rec rec_f x =
    match Hashtbl.find table x with
    | Some v -> v
    | None ->
        let v = f rec_f x in
        Hashtbl.set table ~key:x ~data:v;
        v
  in
  rec_f
