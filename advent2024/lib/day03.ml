open Core

let re_stmt = 
    let open Re in 
    seq [str "mul("; rep1 digit; str ","; rep1 digit; str ")" ]
    |> compile

let re_do_or_dont =
    let open Re in 
    alt [
    seq [str "mul("; rep1 digit; str ","; rep1 digit; str ")" ];
    str "do()"; str "don't()"]
    |> compile

let re_digit = 
    let open Re in 
    seq [repn digit 1 (Some 3)]
    |> compile

let convert_digits mul_stmt = 
    Re.matches re_digit mul_stmt
    |> List.map ~f:Int.of_string

let multiply_digits digits =
    match digits with 
    | [x; y] -> x * y
    | _ -> 0

let sum_multiples (line: string) : int = 
    Re.matches re_stmt line 
    |> List.map ~f:(fun s -> s |> convert_digits |> multiply_digits)
    |> List.fold ~f:(+) ~init:0

let sum_multiples_do_or_dont (line: string) : int = 
    Re.matches re_do_or_dont line 
    |> List.map ~f:(fun s -> s |> convert_digits |> multiply_digits)
    |> List.fold ~f:(+) ~init:0

let sum_multiples_active stmts = 
    let rec aux lst active accum = (
        match lst with 
        | hd::tl -> (
            if (String.equal hd "don't()")  then
                aux tl false accum
            else if (String.equal hd "do()") then  
                aux tl true accum
            else if active then
                let accum = ((hd |> convert_digits |> multiply_digits) + accum) in 
                aux tl true accum
            else
                aux tl false accum
        )
        | [] -> accum 
    ) in 
    aux stmts true 0
