type var = X | Y | Z

type pc = int

type inst = 
  | INC of var
  | DEC of var
  | ZERO of var * pc * pc
  | STOP

let var_to_string v = match v with
  | X -> "x" | Y -> "y" | Z -> "z"

let inst_to_string i = match i with
  | INC v -> "inc " ^ (var_to_string v)
  | DEC v -> "dec " ^ (var_to_string v)
  | ZERO (v,pc,pc') -> "zero " ^ (var_to_string v) ^ " " ^ 
      (string_of_int pc) ^ " else " ^ (string_of_int pc')
  | STOP -> "stop"

let prettyprint is = 
  let rec printlist line is = match is with
    | [] -> print_newline ()
    | i::is' ->
	Printf.printf "%i: %s" line (inst_to_string i);
	print_newline ();
	printlist (line+1) is' in
    printlist 1 is
