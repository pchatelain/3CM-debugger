open Ast

let run prog input trace =

  let step (pc,xval,yval,zval) = match List.nth prog (pc-1) with
    | INC X -> Some (pc+1,xval+1,yval,zval)
    | INC Y -> Some (pc+1,xval,yval+1,zval)
    | INC Z -> Some (pc+1,xval,yval,zval+1)

    | DEC X -> if xval>0 then Some (pc+1,xval-1,yval,zval) else None
    | DEC Y -> if yval>0 then Some (pc+1,xval,yval-1,zval) else None
    | DEC Z -> if zval>0 then Some (pc+1,xval,yval,zval-1) else None

    | ZERO (X,pc',pc'') -> if xval=0
                           then Some (pc',xval,yval,zval) 
                           else Some (pc'',xval,yval,zval)
    | ZERO (Y,pc',pc'') -> if yval=0
                           then Some (pc',xval,yval,zval) 
                           else Some (pc'',xval,yval,zval)
    | ZERO (Z,pc',pc'') -> if zval=0
                           then Some (pc',xval,yval,zval) 
                           else Some (pc'',xval,yval,zval)
    | _ -> None
  in
    
  let rec iterate s =
    (if trace
     then
       match s with | (pc,xval,yval,zval) ->
	 Printf.printf "Trace: (%i,%i,%i,%i)" pc xval yval zval;
	 print_newline ()
     else ());
    match step s with
      | Some s' -> iterate s'
      | None -> match s with
	  | (pc,0,res,0) ->
	      Printf.printf "Result: %i" res;
	      print_newline ()
	  | (pc,xval,yval,zval) -> 
	      Printf.printf "Machine stuck at: (%i,%i,%i,%i)" pc xval yval zval;
	      print_newline ()
  in

    iterate (1,input,0,0)
