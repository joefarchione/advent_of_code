open! Core
open! Core_unix

let read_line filepath = In_channel.read_lines filepath |> List.hd_exn

let write string filepath =
  let oc = Out_channel.create filepath in
  Out_channel.output_string oc string;
  Out_channel.close oc

let request url session_cookie =
  let write_buff = Buffer.create 1763 in
  let connection = Curl.init () in
  try
    Curl.set_writefunction connection (fun x ->
        Buffer.add_string write_buff x;
        String.length x);
    Curl.set_url connection url;
    (* Set the session cookie in the headers *)
    Curl.set_httpheader connection [ "Cookie: session=" ^ session_cookie ];
    Curl.perform connection;
    Curl.global_cleanup ();
    Buffer.contents write_buff
  with _ ->
    Curl.global_cleanup ();
    failwith ("Failed to fetch input from " ^ url)

let file_exists filepath =
  match Sys_unix.file_exists filepath with
  | `No | `Unknown -> false
  | `Yes -> true

let create_newdir path perm =
  if not (file_exists path) then Core_unix.mkdir path ~perm

let get_aoc_input_my_cookie year day =
  let filepath =
    Printf.sprintf "%s/aoc/%d/day%d.txt" (Sys.getenv_exn "HOME") year day
  in
  if file_exists filepath then In_channel.read_all filepath |> String.strip
  else
    let session_cookie =
      read_line (Printf.sprintf "%s/aoc/.aoc_cookie" (Sys.getenv_exn "HOME"))
    in
    let url =
      Printf.sprintf "https://adventofcode.com/%d/day/%d/input" year day
    in
    let input = request url session_cookie in
    create_newdir (Filename.dirname filepath) 0o700;
    write input filepath;
    input
