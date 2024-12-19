open Core


module Position = struct 
  type t = {x:int;y:int}
  let abs t = {x=Int.abs t.x; y = Int.abs t.y}
  let (-) a b = {x=a.x - b.x; y= a.y - b.y}
  let are_adjacent a b = 
    let delta = abs (a-b) in 
    (* vertical, horizontal *)
    ((delta.x = 0) && (delta.y = 1)) 
    || ((delta.x = 1) && (delta.y = 0))


  let find_plot pos positions = 
    let rec walk' pos positions plot other = 
      match positions with 
      | [] -> (plot, other)
      | hd :: tl when are_adjacent pos hd -> (
        let (plot', other') = walk' hd tl plot [] in 
        walk' pos other' (hd :: plot') other
      )
      | hd :: tl -> walk' pos tl plot (hd::other)
    in 
    walk' pos positions [pos] []

  let calculate_plot_perimeter (positions: t list) = 
    let number_adjacent (position: t) (positions: t list) = 
      List.filter positions ~f:(are_adjacent position) 
      |> List.length in 

    List.fold_left positions ~init:0 ~f:(fun acc p -> 
      let num_adj = (number_adjacent p positions) in 
      (acc + Int.(4 - num_adj))
    )

  let calc_cost key positions = 
    let rec calc_cost' positions acc = 
      match positions with 
      | hd::tl -> (
        let (plot, remaining) = find_plot hd tl in 
        let area = List.length plot in 
        let perimeter = calculate_plot_perimeter plot in 
        let cost = area * perimeter in 
        Printf.printf "%c %d * %d = %d\n" key area perimeter (area * perimeter);
        calc_cost' remaining (cost + acc)
      )
      | [] -> acc
      in 
    calc_cost' positions 0

end 


module PlantPositionMap = struct 
  module CharMap = Map.Make(Char) 
  include CharMap
  type t = (Position.t list) CharMap.t
end 

let read filepath = 
  In_channel.read_lines filepath
  |> List.foldi 
    ~init: PlantPositionMap.empty
    ~f:(fun x acc l -> 
      String.to_list l 
      |> List.foldi 
        ~init:acc
        ~f:(fun y acc c ->  Map.update acc c ~f:(
          fun v -> match v with | Some (v) -> Position.{x;y;} :: v | None -> [Position.{x;y;}])
      )
    )

let solve_p1 map = 
  Map.fold map ~init:0 ~f:(fun ~key:k ~data:positions acc ->  
    let cost = (Position.calc_cost k positions) in 
    (cost + acc)
  )
  |> Printf.printf "%d"
