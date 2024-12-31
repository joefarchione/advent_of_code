open Core

module type Comparable = sig
  type t [@@deriving sexp, compare]
end

module PrioQueue (Inner: Comparator.S) = struct

  module Elt = struct
    type t = int * Inner.t
    let compare (a, x) (b, y) =
      match Int.compare a b with 0 -> Inner.comparator.compare x y | x -> x

    let sexp_of_t (a, x) =
      Sexp.List [ Int.sexp_of_t a; Inner.comparator.sexp_of_t x ]
  end

  module EltComp = struct
    include Comparator.Make (Elt)
    type t = Elt.t
  end

  type t = Set.M(EltComp).t


end