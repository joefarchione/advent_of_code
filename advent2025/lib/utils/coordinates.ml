open! Core

module Coord2 = struct
  module T = struct
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

    let of_string ?(on = ',') s =
      match String.split (s |> String.strip) ~on with
      | [ a; b ] -> (Int.of_string a, Int.of_string b)
      | _ -> failwith "Invalid box string"

    let sort (a, b) (x, y) =
      match Int.compare a x with
      | 0 -> (
          match Int.compare b y with
          | 0 -> ((a, b), (x, y))
          | -1 -> ((a, b), (x, y))
          | _ -> ((x, y), (a, b)))
      | -1 -> ((a, b), (x, y))
      | _ -> ((x, y), (a, b))

    let switch (a, b) = (b, a)
  end

  include T
end

module Coord3 = struct
  module T = struct
    type t = int * int * int [@@deriving compare, sexp, hash]
  end

  module Map = Map.Make (T)
  include T

  let of_string ?(on = ',') s =
    match String.split s ~on with
    | [ a; b; c ] -> (Int.of_string a, Int.of_string b, Int.of_string c)
    | _ -> failwith "Invalid box string"
end
