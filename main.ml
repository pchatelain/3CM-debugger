(* main.ml -*- tuareg -*- *)

open Parser
open Analysis

let printf = Printf.printf

  let string_of_token = function
    | INT s -> s
    | X -> "x"
    | Y -> "y"
    | Z -> "z"
    | INC -> "inc"
    | DEC -> "dec"
    | ZERO -> "zero"
    | ELSE -> "else"
    | STOP -> "stop"
    | EOF -> "<EOF>"
    | NEWLINE -> "\n"

  let run chn input trace =
      let lexbuf = Lexing.from_channel chn in

      let ast = Parser.main Lexer.token lexbuf in
      let () = 
	print_string "Program:\n\n";
	Ast.prettyprint ast in
      let _ = Wellformed.check ast in
      let _ = Eval.run ast input trace in
	()

  let _ =
    let input = ref 0
    and trace = ref false
      Arg.parse
	[("-input", Arg.Set_int input, "Input value of x, default is 0");
	 ("-trace", Arg.Set trace, "Trace machine execution, default is false");]
	(fun s -> try
	   let inch = open_in s in
	     print_string ("Opening file \"" ^ s ^ "\"\n\n");
	     run inch !input !trace;
	     close_in inch;
	     exit 0
	 with Sys_error e -> raise (Arg.Bad e)
	) "Usage: 3cm [options] <filename>"
	
