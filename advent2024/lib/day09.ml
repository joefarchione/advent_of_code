open Core

module FreeCode = struct
  type t = int
  let next t = if t > 1 then Some (t-1) else None


  let to_string t = 
    let open Utils in 
    List.fold (1--t) ~init:"" ~f:(fun acc _ -> acc ^ ("."))
end

module BlockCode = struct 
  type t = {length: int; value: int} [@@deriving sexp, compare, equal]
  let next t = 
    if t.length > 1 then 
      Some {t with length = t.length - 1}
    else 
        None

  let to_string t = 
    let open Utils in 
    List.fold (1--t.length) ~init:"" ~f:(fun acc _ -> acc ^ (Printf.sprintf "%d" t.value))
end

module BlockSet = Set.Make(BlockCode)

module DiskMap = struct
  type encoding = Block of BlockCode.t | Free of FreeCode.t
  type t  = encoding list

  let print t = 
    let open Utils in
    List.iter t ~f:(function
    | Free (f) -> List.iter (1--f) ~f:(fun _ -> Printf.printf ".")
    | Block (b) -> List.iter (1--b.length) ~f:(fun _ -> Printf.printf "%d" b.value)
    )

  let length t = 
    List.fold t ~init:0 ~f:(fun acc a ->
      match a with
      | Free (f) -> f + acc
      | Block (b) -> b.length + acc
    )

  let take_block (t: encoding list) = 
    let rec aux t free_count = 
      match t with 
      | Free (f) :: tl -> aux tl (free_count+f)
      | Block (block) :: tl -> (
        match BlockCode.next block with 
        | Some (b) ->  (Some block, Block (b) :: tl, free_count)
        | None -> (Some block, tl, free_count)
      )
      | [] -> (None, [], free_count)
    in
    aux t 0

  let remove_first_matching predicate list =
    let rec aux acc = function
      | [] -> None, List.rev acc
      | x :: xs -> if predicate x then Some x, List.rev acc @ xs else aux (x :: acc) xs
    in
    aux [] list

  let take_block_of_size n t = 
    let (block, t) = 
      remove_first_matching 
        (function | Free (_) -> false | Block(b) -> b.length = n) 
        t
    in
    match block with 
    | Some (Free (_)) -> failwith "only need blocks!"
    | Some (Block (b)) -> (Some b, t)
    | None -> (None, t)

  let find_free_space_of_at_most_size n t = 
    let (block, t) = 
      remove_first_matching 
        (function | Free (f) -> f <= n | Block(_) -> false) 
        t
    in
    match block with 
    | Some (Block (_)) -> failwith "only need freespace!"
    | Some (Free (f)) -> (Some f, t)
    | None -> (None, t)

  

  let rec read_disk_map ?(index=0) digits = 
    match digits with 
    | block_size::0::tl -> (
        (Block BlockCode.{length=block_size; value=index}) :: read_disk_map ~index:(index + 1) tl
    )
    | block_size::space_size::tl -> (
        (Block BlockCode.{length=block_size; value=index}) :: Free (space_size) :: read_disk_map ~index:(index + 1) tl
    )
    | [block_size] -> (Block {length=block_size; value=index}) :: read_disk_map ~index:(index + 1) []
    | [] -> []

  let calculate_check_sum_moving_blocks diskmap = 
    let rec aux map rev_map position checksum length nfree =
      match map with 
      | Free (f) :: tl when (length > ((position) + (nfree)))  -> (
        let (block, rev_map, free_from_rev) = take_block rev_map in 
        match block with 
        | Some (b) -> (
          let map = (
            match FreeCode.next f with 
            | Some (v) -> Free (v) :: tl
            | None -> tl
          ) in
          if (length > ((position) + (nfree + free_from_rev))) then
            aux map rev_map (position+1) ((position * b.value) + checksum) length (nfree+1+free_from_rev)
          else 
            checksum
        )
        | None -> checksum
      )
      | Block (b) :: tl when (length > ((position) + nfree)) ->  (
        let map = match BlockCode.next b with
        | Some (v) -> Block (v) :: tl
        | None -> tl in 
        aux map rev_map (position+1) ((position * b.value) + checksum) length nfree
      )
      | _ -> checksum
    in 
    aux diskmap (List.rev diskmap) 0 0 (length diskmap) 0


  let calculate_check_sum_on_sort sorted = 
    let rec aux' position sorted acc = 
      match sorted with 
      | Free (f) :: tl -> aux' (position + f) tl acc
      | Block (b) :: tl -> 
        let open Utils in 
        let delta = List.map (0--(b.length-1)) ~f:(fun i -> (position + i) * b.value) |> List.fold ~init:0 ~f:(+) in 
        aux' (position + b.length) tl (acc + delta)
      | _ -> acc
    in
    aux' 0 sorted 0

  let calculate_checksum_on_files diskmap = 
    let rec replace' map (block: BlockCode.t) (sorted: encoding list) = 
      match map with 
      | Free (f) :: _ when f >= block.length ->  (
        if f - block.length > 0  then 
          Free (f-block.length) :: Block (block) :: sorted 
        else
          Block (block) :: sorted 
      )
      | hd::tl -> replace' tl block (hd::sorted)
      | [] -> sorted
    in 

    let rev_diskmap = List.rev diskmap 
    |> List.filter ~f:(function | Block (_) -> true | Free _ -> false) 
    |> List.map ~f:(function | Block (b) -> b | Free _ -> failwith "no blocks!") in 

    let sorted = List.fold rev_diskmap ~init:[] ~f:(fun acc block -> replace' diskmap block acc)
    |> List.rev in 

    List.iter sorted ~f:(fun code -> 
      (match code with 
      | Free (f) -> Printf.printf "%s" (FreeCode.to_string f)
      | Block (b) -> Printf.printf "%s" (BlockCode.to_string b));
    );
    print_endline "";

    calculate_check_sum_on_sort sorted

end

let read filepath = 
  filepath 
  |> In_channel.read_lines 
  |> List.hd
  |> function 
    | Some (l) -> (
      String.to_list l
      |> List.map ~f:Char.get_digit_exn
      |> DiskMap.read_disk_map  
    )
    | None -> failwith "empty file"

let solve_p1 diskmap = 
  DiskMap.calculate_check_sum_moving_blocks diskmap
  |> (Printf.printf "%d")


let solve_p2 diskmap = 
  DiskMap.calculate_checksum_on_files diskmap
  |> Printf.printf "%d"