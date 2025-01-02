open Core


type fileblock  = {id:int; length:int} [@@deriving sexp, equal, compare]
type free  = {length: int;} [@@deriving sexp, equal, compare]
type disktype = FileBlock of fileblock | Free of free [@@deriving sexp, equal, compare]
type disk = disktype list [@@deriving sexp, equal, compare]

let take_from_fileblock (block: fileblock) = 
  if block.length = 0 then failwith "empty fileblock"
  else if block.length = 1 then None
  else Some {block with length = Int.pred block.length}

let take_from_free_n n (free: free) = 
  if free.length = 0 then failwith "empty space"
  else if free.length = n then None
  else Some {length = free.length - n}

  
let take_from_free (free: free) = take_from_free_n 1 free

let can_fit_fileblock (free:free) (fileblock:fileblock) = fileblock.length <= free.length

module DiskDeque = struct 

  module M = Deque

  type t = disktype Deque.t

  let create () = Deque.create ()

  let deque_front_or_back_item deque enque (t:t) = 
    match deque t with 
    | Some (item) -> (
      match item with 
      | Free (free) ->  (
          match take_from_free free with 
          | Some (remaining) -> enque t (Free remaining)
          | None -> ());
          Some (Free free)
      | FileBlock (file) -> (
          match take_from_fileblock file with 
          | Some (remaining) -> enque t (FileBlock remaining)
          | None -> ());
          Some (FileBlock file)
    )
    | None -> None

  let deque_front_item = deque_front_or_back_item Deque.dequeue_front Deque.enqueue_front 
  let deque_back_item = deque_front_or_back_item Deque.dequeue_back Deque.enqueue_back 

  let rec deque_back_fileblock (t:t) = 
    match Deque.dequeue_back t with 
    | None -> None
    | Some (disktype) -> (
      match disktype with 
      | Free _ -> deque_back_fileblock t
      | FileBlock (block) -> Some block
    )

  let rec enque_front_space_n n (t:t) = 
    match Deque.dequeue_back t with 
    | None -> None
    | Some (disktype) -> (
      match disktype with 
      | Free _ -> deque_back_fileblock t
      | FileBlock (_) -> enque_front_space_n n t
    )

end

let read filepath = 
  let re_digit = (Re.digit |> Re.compile) in 
  let deque = DiskDeque.create () in 
  In_channel.read_lines filepath
  |> List.hd_exn
  |> Re.matches re_digit
  |> List.map ~f:(int_of_string)
  |> List.iteri ~f:(fun i d -> 
    if (i mod 2) = 0 then 
      Deque.enqueue_back deque (FileBlock {id = i/2; length = d})
    else if d = 0 then ()
    else Deque.enqueue_back deque (Free {length = d})
  );
  deque

let read_p2 filepath = 
  let re_digit = (Re.digit |> Re.compile) in 

  let disk = 
    In_channel.read_lines filepath
    |> List.hd_exn
    |> Re.matches re_digit
    |> List.map ~f:(int_of_string)
    |> List.foldi ~init:[] ~f:(fun i acc d -> 
      if (i mod 2) = 0 then 
        (FileBlock {id = i/2; length = d})::acc
      else if d = 0 then acc
      else (Free {length = d})::acc
    )
    in
  (
    List.rev disk, 
    disk 
    |> List.filter ~f:(function | Free _ -> false | FileBlock _ -> true)
    |> List.map ~f:(function | Free (_) -> failwith "no frees" | FileBlock (f) -> f)
  )

let update_checksum checksum n id = checksum + (n * id)

let calculate_checksum disk = 
  let rec aux n disk checksum = 
    match DiskDeque.deque_front_item disk with 
    | Some (FileBlock (file)) -> (
        aux (n+1) disk (update_checksum checksum n file.id)
    )
    | Some (Free (_)) -> (
      let file = DiskDeque.deque_back_fileblock disk in 
      match file with 
      Some (file) -> (
        aux (n+1) disk (update_checksum checksum n file.id) 
      )
      | None -> checksum
    )
    | None -> checksum
  in
  let output = aux 0 disk 0 in 
  print_endline "";
  output

let solve_p1 disk = 
  calculate_checksum disk
  |> Printf.printf "%d"

