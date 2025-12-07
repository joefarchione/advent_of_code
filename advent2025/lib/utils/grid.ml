open! Core

type dimensions = (int * int[@deriving sexp, compare, hash])

let rows = fst
let cols = snd
let create (n, m) : dimensions = (n, m)

let contains (n, m) (x, y) =
  let open Int in
  x >= 0 && x < n && y >= 0 && y < m
