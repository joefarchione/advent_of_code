open! Core

let read_line filepath = In_channel.read_lines filepath |> List.hd_exn
