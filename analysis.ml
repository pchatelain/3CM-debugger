open Ast
open Interval

let union_f f1 f2 =
  let union_3 (a,b,c) (d,e,f) = (union a d),(union b e),(union c f) in
    fun x -> union_3 (f1 x) (f2 x);;

let intersection_f f1 f2 =
  let intersection_3 (a,b,c) (d,e,f) = (intersection a d),(intersection b e),(intersection c f) in
    fun x -> intersection_3 (f1 x) (f2 x);;

let widen_f f1 f2 =
  let widen_3 (a,b,c) (d,e,f) = (widen a d),(widen b e),(widen c f) in
    fun x -> widen_3 (f1 x) (f2 x);;

let narrow_f f1 f2 =
  let narrow_3 (a,b,c) (d,e,f) = (narrow a d),(narrow b e),(narrow c f) in
    fun x -> narrow_3 (f1 x) (f2 x);;
	
let forward s prog =
  let next = ref (function 1 -> (top ()),Interval (0,0),Interval (0,0) | _ -> Empty,Empty,Empty) in
    for pc = 1 to (List.length prog) do
      match List.nth prog (pc - 1) with
	  INC X -> next := union_f (!next) (fun x -> if (x == pc+1)
					    then (x_plus (s pc))
					    else Empty,Empty,Empty)
	| INC Y -> next := union_f (!next) (fun x -> if (x == pc+1)
					    then (y_plus (s pc))
					    else Empty,Empty,Empty)
	| INC Z -> next := union_f (!next) (fun x -> if (x == pc+1)
					    then (z_plus (s pc))
					    else Empty,Empty,Empty)
	| DEC X -> next := union_f (!next) (fun x -> if (x == pc+1)
					    then (x_minus (s pc))
					    else Empty,Empty,Empty)
	| DEC Y -> next := union_f (!next) (fun x -> if (x == pc+1)
					    then (y_minus (s pc))
					    else Empty,Empty,Empty)
	| DEC Z -> next := union_f (!next) (fun x -> if (x == pc+1)
					    then (z_minus (s pc))
					    else Empty,Empty,Empty)
	| ZERO (X,pc',pc'') -> (next := union_f (!next)
				  (fun x -> if (x == pc')
				   then x_zero (s pc)
				   else Empty,Empty,Empty);
				next := union_f (!next)
				  (fun x -> if (x == pc'')
				   then x_non_zero (s pc)
				   else Empty,Empty,Empty))
	| ZERO (Y,pc',pc'') -> (next := union_f (!next)
				  (fun x -> if (x == pc')
				   then y_zero (s pc)
				   else Empty,Empty,Empty);
				next := union_f (!next)
				  (fun x -> if (x == pc'')
				   then y_non_zero (s pc)
				   else Empty,Empty,Empty))
	| ZERO (Z,pc',pc'') -> (next := union_f (!next)
				  (fun x -> if (x == pc')
				   then z_zero (s pc)
				   else Empty,Empty,Empty);
				next := union_f (!next)
				  (fun x -> if (x == pc'')
				   then z_non_zero (s pc)
				   else Empty,Empty,Empty))
	| _ -> ()
    done;
    !next;;

let backward s prog =
  let next = ref (fun pc -> Interval (0,0),(top ()),Interval (0,0)) in
    for pc = 1 to (List.length prog) do
      match List.nth prog (pc - 1) with
	  INC X -> next := union_f (!next) (fun x -> if (x == pc)
					    then (x_minus (s (pc+1)))
					    else Empty,Empty,Empty)
	| INC Y -> next := union_f (!next) (fun x -> if (x == pc)
					    then (y_minus (s (pc+1)))
					    else Empty,Empty,Empty)
	| INC Z -> next := union_f (!next) (fun x -> if (x == pc)
					    then (z_minus (s (pc+1)))
					    else Empty,Empty,Empty)
	| DEC X -> next := union_f (!next) (fun x -> if (x == pc)
					    then (x_plus (s (pc+1)))
					    else Empty,Empty,Empty)
	| DEC Y -> next := union_f (!next) (fun x -> if (x == pc)
					    then (y_plus (s (pc+1)))
					    else Empty,Empty,Empty)
	| DEC Z -> next := union_f (!next) (fun x -> if (x == pc)
					    then (z_plus (s (pc+1)))
					    else Empty,Empty,Empty)
	| ZERO (X,pc',pc'') -> (next := union_f (!next)
				  (fun x -> if (x == pc)
				   then x_zero (s pc')
				   else Empty,Empty,Empty);
				next := union_f (!next)
				  (fun x -> if (x == pc)
				   then x_non_zero (s pc'')
				   else Empty,Empty,Empty))
	| ZERO (Y,pc',pc'') -> (next := union_f (!next)
				  (fun x -> if (x == pc)
				   then y_zero (s pc'')
				   else Empty,Empty,Empty);
				next := union_f (!next)
				  (fun x -> if (x == pc)
				   then y_non_zero (s pc'')
				   else Empty,Empty,Empty))
	| ZERO (Z,pc',pc'') -> (next := union_f (!next)
				  (fun x -> if (x == pc)
				   then z_zero (s pc')
				   else Empty,Empty,Empty);
				next := union_f (!next)
				  (fun x -> if (x == pc)
				   then z_non_zero (s pc'')
				   else Empty,Empty,Empty))
	| _ -> ()
    done;
    !next;;

let equal_3 (a,b,c) (d,e,f) =
  (equal a d) & (equal b e) & (equal c f);;

let equal_f s1 s2 prog =
  let eq = ref true in
    for pc = 1 to (List.length prog) do
	eq := (!eq) & (equal_3 (s1 pc) (s2 pc))
    done;
    !eq;;

let lfp prog =
  let rec lfp_rec prog previous =
    let next = forward previous prog in
    let widened = widen_f previous next in
      if (equal_f widened previous prog) then
	previous
      else
	lfp_rec prog widened
  in
    lfp_rec prog (fun x -> Empty,Empty,Empty);;

let assert_dec prog = fun
  pc -> if (pc <= List.length prog) then
    match List.nth prog (pc - 1) with
	DEC Y -> (top ()),(Interval (1,max_int)),(top ())
      | _ -> (top ()),(top ()),(top ())
  else
    (top ()),(top ()),(top ());;

let constrained_backward s prog =
  intersection_f (assert_dec prog) (backward s prog);;

let gfp prog =
  let rec gfp_rec prog previous =
    let next = constrained_backward previous prog in
    let narrowed = narrow_f previous next in
      if (equal_f narrowed previous prog) then
	previous
      else gfp_rec prog narrowed
  in
    gfp_rec prog (fun x -> (top ()),(top ()),(top ()));;

let print_analysis prog =
  let analysis = gfp prog in
    for pc = 1 to (List.length prog) do
      let (x,y,z) = analysis pc in
      let sx = string_of_interval x
      and sy = string_of_interval y
      and sz = string_of_interval z in
      let s = "{x:"^sx^",y:"^sy^",z:"^sz^"}" in
	Printf.printf "%3i: %-20s %-20s"
	  pc
	  (inst_to_string (List.nth prog (pc-1)))
	  s;
	print_newline();
    done;;

let print_10_steps prog =
  let analysis = ref (fun x -> Empty,Empty,Empty)
  and widened = ref (fun x -> Empty,Empty,Empty) in
  for i = 0 to 10 do
    analysis := forward (!widened) prog; (*lfp prog*)
      for pc = 1 to (List.length prog) do
	let (x,y,z) = (!analysis) pc in
	let sx = string_of_interval x
	and sy = string_of_interval y
	and sz = string_of_interval z in
	let s = "{x:"^sx^",y:"^sy^",z:"^sz^"}" in
	  Printf.printf "%3i: %-20s %-20s"
	    pc
	    (inst_to_string (List.nth prog (pc-1)))
	    s;
	  print_newline();
      done;
	  print_newline();
  widened := widen_f (!widened) (!analysis);
    for pc = 1 to (List.length prog) do
	let (x,y,z) = (!widened) pc in
	let sx = string_of_interval x
	and sy = string_of_interval y
	and sz = string_of_interval z in
	let s = "{x:"^sx^",y:"^sy^",z:"^sz^"}" in
	  Printf.printf "%3i: %-20s %-20s"
	    pc
	    (inst_to_string (List.nth prog (pc-1)))
	    s;
	  print_newline();
      done;
	  print_newline();
done;;
  
