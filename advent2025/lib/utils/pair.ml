open! Core

module Coord2d = struct
  include Tuple2

  type t = int * int [@@deriving sexp, compare, hash]

  include Tuple.Comparable (Int) (Int)

  let up t = (fst t - 1, snd t)
  let down t = (fst t + 1, snd t)
  let left t = (fst t, snd t - 1)
  let right t = (fst t, snd t + 1)
  let up_left t = up (left t)
  let up_right t = up (right t)
  let down_left t = down (left t)
  let down_right t = down (right t)

  let neighbors t =
    [
      up t;
      down t;
      left t;
      right t;
      up_left t;
      up_right t;
      down_left t;
      down_right t;
    ]

  let in_grid n m (x, y) =
    let open Int in
    x >= 0 && x < n && y >= 0 && y < m
end

module Coord2Set = Set.Make (Coord2d)

module Coord3d = struct
  type t = int * int * int [@@deriving compare, sexp, hash]
end

module Coord3dMap = Map.Make (Coord3d)
module Coord3dSet = Set.Make (Coord3d)
