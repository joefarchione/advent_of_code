open! Core

type register_value = int

type program = {
  a: register_value; 
  b: register_value; 
  c: register_value; 
  pointer: int;
  instructions: int list;
  opcode: int;
  operand: int;
}

let program_information = 
  let open Re in
  seq [
    bol; str "Register A: "; group (rep1 digit); eol;
    bol; str "Register B: "; group (rep1 digit); eol;
    bol; str "Register C: "; group (rep1 digit); eol;
    bol; eol;
    bol; str "Program: "; group (seq [rep1 digit; opt (str ",");]); eol;
  ] 

let create a b c instructions = 
  let opcode = List.nth_exn instructions 0 in 
  let operand = List.nth_exn instructions 1 in 
  {
    a;b;c;instructions; pointer=0; opcode; operand;
  }

exception InvalidComboOperand
exception Halt

let eval_combo program combo = 
  match combo with 
  | 0 | 1 | 2 | 3 -> combo
  | 4 -> program.a
  | 5 -> program.b
  | 6 -> program.c
  | _ -> raise @@ InvalidComboOperand

let get program position = 
  match List.nth program.instructions position with 
  | None -> raise @@ Halt
  | Some (v) -> v

let next program = 
  let next_position = program.pointer + 2 in 
  {program with 
    pointer = next_position; 
    opcode = (get program next_position); 
    operand = (get program (next_position + 1)); }

let get n program = {program with pointer = n; opcode = (get program n); operand = (get program (n + 1)); }
let adv program = next {program with a = program.a / (Int.pow 2 (eval_combo program program.operand))}
let bxl program = next {program with b = Int.bit_xor program.b program.operand}
let bst program = next {program with b = (eval_combo program program.operand) mod 8}
let jnz program = if Int.equal program.a 0 then next program else (get program.operand program)
let bxc program = next {program with b = Int.bit_xor program.b program.c}
let out program = Printf.printf "%d," ((eval_combo program program.operand) mod 8); next program
let bdv program = next {program with b = program.a / (Int.pow 2 (eval_combo program program.operand))}
let cdv program = next {program with c = program.a / (Int.pow 2 (eval_combo program program.operand))}
let interpret_opcode program =
  match program.opcode with
  | 0 -> adv program
  | 1 -> bxl program
  | 2 -> bst program
  | 3 -> jnz program
  | 4 -> bxc program
  | 5 -> out program
  | 6 -> bdv program
  | 7 -> cdv program
  | _ -> failwith "_"

let interpret program =
  let rec interpret' program  =
    interpret' (interpret_opcode program)
  in

  try 
    interpret' program
  with 
  | InvalidComboOperand -> Printf.printf "Invalid combo operation"
  | Halt -> Printf.printf "Halt!"
