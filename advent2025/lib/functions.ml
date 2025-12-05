open! Core

let read_line filepath = In_channel.read_lines filepath |> List.hd_exn

module List = struct
  include List

  let split_on ~on lst =
    List.split_while lst ~f:(fun x -> not (on x)) |> fun (before, after) ->
    (before, List.drop after 1)
end
