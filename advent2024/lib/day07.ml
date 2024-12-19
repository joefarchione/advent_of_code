open! Core

module Operators = struct
  type t = | Add | Mul | Concat

  let permutations n = Utils.permutations n
  
  let eval t a b = 
    match t with 
    | Add -> a + b
    | Mul -> a * b
    | Concat -> int_of_string ((string_of_int a) ^ (string_of_int b))

end

module CalibEq = struct 

  type t = {target: int; numbers: int list; num_ops: int }

  let create target numbers = {target; numbers; num_ops = (List.length numbers) - 1}

  let from_string input = 
    let re_digit = (Re.rep1 Re.digit) |> Re.compile in 
    let int_lst =  Re.matches re_digit input |> List.map ~f:Int.of_string  in 
    match int_lst with 
    | hd::tl -> create hd tl
    | _ -> failwith "couldn't parse target"

  let eval ops numbers = 
      match numbers with 
      | hd::tl -> 
        Some (List.fold2_exn ops tl ~init:hd ~f:(fun acc op a -> Operators.eval op acc a))
      | _ -> None

  let solve ops t = 
    List.exists 
      (Operators.permutations t.num_ops ops)
      ~f:(fun ops -> 
        match eval ops t.numbers with 
        | Some (v) -> Int.equal v t.target 
        | None -> false) 

  let solve_equations ops equations = 
    List.filter equations ~f:(solve ops )
    |> List.fold ~init:0 ~f:(fun acc eq -> acc + eq.target)

end


let solve_p1 = CalibEq.solve_equations [Operators.Add; Operators.Mul;]
let solve_p2 = CalibEq.solve_equations [Operators.Add; Operators.Mul; Operators.Concat;]
