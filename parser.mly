/* File parser.mly */
%{

  let convert_negint s =
    let len = String.length s in
    let s' = String.sub s 1 (len-1) in
    - (int_of_string s')

%}
%token <string> INT
%token X
%token Y
%token Z
%token INC
%token DEC
%token ZERO
%token ELSE
%token STOP
%token EOF
%token NEWLINE

%left 

%start main             /* the entry point */
%type <Ast.inst list> main
%%

main :  instlist EOF            { $1 }
     |  EOF                     { Printf.printf "exiting\n"; exit 0 }

;

instlist : STOP NEWLINE         { [Ast.STOP] }
         | inst NEWLINE instlist { $1::$3 }
;

inst : INC var                  { Ast.INC $2 }
     | DEC var                  { Ast.DEC $2 }
     | ZERO var INT ELSE INT    { Ast.ZERO ($2, 
					    int_of_string $3,
					    int_of_string $5) } 
;

var : X                         { Ast.X }
    | Y                         { Ast.Y }
    | Z                         { Ast.Z }
;
