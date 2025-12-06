open! Core
include Sequence

let range_inclusive a b = Sequence.init (b - a + 1) ~f:(( + ) a)
let range_exclusive a b = Sequence.init (b - a) ~f:(( + ) a)
