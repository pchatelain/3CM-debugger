open Ast

let check prog =
  let maxinst = List.length prog in

  let check_inst i = match i with
    | INC v -> true
    | DEC v -> true
    | ZERO (v,pc,pc') -> 
	if pc > maxinst || pc' > maxinst || pc <= 0 || pc' <= 0
	then
	  (Printf.printf "Offset out of bounds: %s" (Ast.inst_to_string i);
	   print_newline ();
	   false)
        else true
    | STOP -> true  in

  let rec check_offsets is accres = match is with
    | [] -> accres
    | i::is' -> 
	let res = check_inst i in
	  check_offsets is' (res && accres) in

    check_offsets prog true
