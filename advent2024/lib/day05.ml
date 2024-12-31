open Core

module Page = struct type t = int [@@deriving sexp, equal, compare] end
module PageSet = Set.Make(Page)

module PageRulesBefore = struct
  module M = Map.Make(Page)
  include M

  type t = PageSet.t M.t

  let from_lines lines = 
    let rec from_lines' lines mapping = 
      match lines with 
      | (a, b)::tl -> (
        let mapping = Map.update mapping a ~f:(function | Some (v) -> (Set.add v b) | None -> (Set.add PageSet.empty b)) in 
        from_lines' tl mapping
      )
      | [] -> mapping
    in
    from_lines' lines empty

  let can_be_before t page page_before = 
    match Map.find t page with 
    | None -> true
    | Some (pages) -> not (Set.mem pages page_before)
end


module PageOrdering = struct 
  type t = Page.t list [@@deriving sexp, equal, compare] 
  let get_middle_exn t = 
    let length = (List.length t - 1) in 
    let middle = if length % 2 = 0 then length / 2 else length / 2 + 1 in 
    List.nth_exn t middle

  let rec check_rule rules (pages_before: t) (page: Page.t) = 
    match pages_before with 
    | page_before::other_pages when PageRulesBefore.can_be_before rules page page_before -> 
      check_rule rules other_pages page
    | _::_ -> false
    | [] -> true

end

type input = {rules: PageRulesBefore.t; orderings: PageOrdering.t list}

let read filepath =
  let re_digits = (Re.rep1 Re.digit |> Re.compile) in 
  In_channel.read_lines filepath
  |> Utils.split_on_newline 
  |> (fun split -> 
    match split with
    | [lines_rules;lines_orderings] -> (
      let rules = List.map lines_rules 
        ~f:(fun line -> (
            match (Re.matches re_digits line) with 
            | [a;b] -> (int_of_string a, int_of_string b)
            | _ -> failwith "incorrect input"
          )
        )
        |> PageRulesBefore.from_lines
      in 
      let orderings = List.map lines_orderings
        ~f:(fun line -> 
          match Re.matches re_digits line with 
          | [] -> failwith "incorrect input"
          | lst -> (List.map lst ~f:int_of_string)
        )
      in 
      {rules; orderings}
    )
    | _ -> failwith "Incorrect input"
  )


let check_ordering rules (ordering: PageOrdering.t) = 
  let rec aux rules (ordering: PageOrdering.t)  = 
    match ordering with 
    | hd::tl when PageOrdering.check_rule rules tl hd -> aux rules tl
    | _::_ -> false
    | [] -> true
    in 
    aux rules (List.rev ordering)

let check_ordering_move rules (ordering: PageOrdering.t) = 
  let rec aux rules (ordering: PageOrdering.t) new_ordering swapped  = 
    match ordering with 
    | hd::tl when PageOrdering.check_rule rules tl hd -> aux rules tl (hd::new_ordering) swapped
    | a::b::tl -> aux rules ((b::tl)@[a]) new_ordering true
    | a::[] when swapped -> Some (a::new_ordering) 
    | [] when swapped -> Some new_ordering
    | _ -> None
    in 
    aux rules (List.rev ordering) [] false 

let solve_p1 input = 
  input.orderings 
  |> List.filter ~f:(check_ordering input.rules)
  |> List.map ~f:PageOrdering.get_middle_exn
  |> List.fold ~init:0 ~f:(+)
  |> Printf.printf "%d"

let solve_p2 input = 
  input.orderings 
  |> List.filter_map ~f:(check_ordering_move input.rules)
  |> List.map ~f:PageOrdering.get_middle_exn
  |> List.fold ~init:0 ~f:(+)
  |> Printf.printf "%d"