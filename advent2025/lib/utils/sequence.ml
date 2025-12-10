open! Core
include Sequence

let range_inclusive a b = Sequence.init (b - a + 1) ~f:(( + ) a)
let range_exclusive a b = Sequence.init (b - a) ~f:(( + ) a)

let rec n_cartesian_product = function
  | [] ->
      Sequence.singleton []
      (* Base case: product of an empty list of sequences is a sequence containing a single empty list *)
  | x :: xs ->
      (* Head x is the current sequence to combine, tail xs is the rest *)
      let rest_products = n_cartesian_product xs in
      (* Recursively get products of the remaining sequences *)
      Sequence.concat (* Flatten the result of the mapping *)
        (Sequence.map (* For each element i in the current sequence x *) x
           ~f:(fun i ->
             Sequence.map (* For each product list rs from the rest *)
               rest_products ~f:(fun rs -> i :: rs)))
(* Prepend i to the front of the product list *)
