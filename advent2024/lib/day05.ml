open Core

module IntSet = Set.Make(Int)

module OrderingRules = struct
  module M = Map.Make(Int)
  include M

  let from_tuples (pairs: (int*int) list) = 
    let rec aux (pairs: (int*int) list) map = 
      match pairs with 
      | (a,b)::tl -> (
        let map = Map.update map a ~f:(fun v_opt -> 
            match v_opt with
            | Some (v) -> (Set.add v b)
            | None -> (Set.add IntSet.empty b)
        )
        in
        aux tl map
      )
      | [] -> map
    in
    let rules = M.empty in 
    aux pairs rules

  let check map before after  = 
    let a = match Map.find map before with 
    | Some (pages) -> (Set.mem pages after)
    | None -> true  in 

    let b = match Map.find map after with 
    | Some (pages) -> not (Set.mem pages before)
    | None -> true  in 

    a && b

end

module PageList = struct

  type t = int list

  let rec check_ordering rules pages = 
    match pages with 
    | hd :: tl -> 
      if List.for_all tl ~f:(OrderingRules.check rules hd) then
        check_ordering rules tl
      else 
        false
    | [] -> true

  let get_middle pages = 
    match List.nth pages ((List.length pages) / 2) with 
    | Some (v) -> v
    | None -> failwith "incorrect index?"

end

let pages = let open Re in seq [rep1 digit] |> compile
let rule = let open Re in seq [bol; group (rep1 digit); str "|"; group (rep1 digit); eol] |> compile 

let find_rules line = 
    let open Re in 
    let matched =  exec_opt rule line in 
    match matched with 
    | Some (m) -> ((Int.of_string (Group.get m 1)), (Int.of_string (Group.get m 2)))
    | None -> failwith "not a rule!"

let find_pages line = 
  Re.matches pages line 
  |> List.map ~f:Int.of_string

let parse_lines lines = 
  let rec aux lines parsing_pages rules pages =
    match lines with 
    | [] -> (OrderingRules.from_tuples rules, pages)
    | hd :: tl -> 
      if (String.equal hd "\n") || (String.equal hd "") then 
        aux tl true rules pages
      else if parsing_pages then (
        let pages = (find_pages hd) :: pages in 
        aux tl parsing_pages rules pages
      )
      else (
        let rules = (find_rules hd) :: rules in
        aux tl parsing_pages rules pages
      )
  in
  aux lines false [] []

let solve_p1 rules page_lists = 
  page_lists
  |> List.filter ~f:(PageList.check_ordering rules) 
  |> List.map ~f:PageList.get_middle
  |> List.fold_left ~f:(fun acc a -> acc + a) ~init:0
  |> Printf.printf "%d";



