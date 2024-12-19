open Core


type position = {x:int; y:int;}

type machine_spec = {a: position; b: position; c:position}

let solve_spec_p1 b1 b2 s = 
  let (a,b,c,d) = (b1.x, b2.x, b1.y, b2.y) in 
  let (s1, s2) = (s.x, s.y) in 

  let det  = (a * d) - (b * c) in 
  let n = (d * s1 - b * s2) / det in 
  let m = ((-c) * s1 + a * s2) / det in 

  if (n <= 100) && (m<=100) then (
    if 
      (Int.equal (b1.x * n + b2.x * m) s1)
      && (Int.equal (b1.y * n + b2.y * m) s2)
    then 
      Some (n , m )
    else
      None
  )
  else
    None

let solve_spec_p2 b1 b2 s = 
  let (a,b,c,d) = (b1.x, b2.x, b1.y, b2.y) in 
  let (s1, s2) = (10000000000000 + s.x, 10000000000000 + s.y) in 

  let det  = (a * d) - (b * c) in 
  let n = (d * s1 - b * s2) / det in 
  let m = ((-c) * s1 + a * s2) / det in 

  if 
    (Int.equal (b1.x * n + b2.x * m) s1)
    && (Int.equal (b1.y * n + b2.y * m) s2)
  then 
    Some (n , m )
  else
    None

  
let read filepath = 
  In_channel.read_lines filepath
  |> List.filter ~f:(fun line -> not (String.equal "" line))
  |> List.chunks_of ~length:3
  |> List.map ~f:(fun lines -> (
      let digits = List.map lines ~f:(fun line -> Re.matches ((Re.rep1 Re.digit) |> Re.compile) line |> List.map ~f:int_of_string) |> List.concat in 
      match digits with 
      | [ax; ay; bx; by; cx; cy] -> {
        a = {x=ax; y = ay;};
        b = {x=bx; y = by;};
        c = {x=cx; y = cy;};
      }
      | _ -> failwith "_"
      )
    )

let solve_p1 input = 
  input
  |> List.fold_left ~init:0 ~f:(fun acc spec -> 
    match solve_spec_p1 spec.a spec.b spec.c with 
    | Some (n, m) -> (n * 3) + m  + acc
    | None -> acc
  )
  |> Printf.printf "%d" 

let solve_p2 input = 
  input
  |> List.fold_left ~init:0 ~f:(fun acc spec -> 
    match solve_spec_p2 spec.a spec.b spec.c with 
    | Some (n, m) -> (n * 3) + m  + acc
    | None -> acc
  )
  |> Printf.printf "%d" 



