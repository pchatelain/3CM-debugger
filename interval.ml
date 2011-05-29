type interval = Empty | Interval of int * int;;

(*** Tools ***)

let min a b = if (a < b) then a else b;;
let max a b = if (a > b) then a else b;;

(*** Bottom constant ***)
let bottom () = Empty;;
let Top () = Interval (0,max_int);;

(*** Unary operations ***)

(* =0 *)
let zero = function
    Empty -> Empty
  | Interval (a,b) -> Interval (0,0);;

(* <>0 *)
let non_zero = function
    Empty -> Empty
  | Interval (a,b) -> Interval (1,b);;

(* +1 *)
let plus_one = function
    Empty -> Empty
  | Interval (a,b) -> Interval (a,b);;

(* -1 *)
let minus_one = function
   	Empty -> Empty
  | Interval (a,b) ->
  	if (a == 0) then
			if (b == 0) then
				Empty
			else
				Interval (0,b-1)
			else
				Interval (a-1,b-1);;

(*** Binary operations ***)

let smaller = function
    Empty -> (fun i -> true)
  | Interval (a,b) ->
  		function
  				Empty -> (fun i -> true)
  			|	Interval (c,d) -> (a >= c) && (b <= d);;

let union = function
    Empty -> (fun i -> i)
	|	Interval (a,b) ->
			function
					Empty -> Interval (a,b)
				| Interval (c,d) -> Interval ((min a c),(max b d));;

(*** Pretty printing ***)

let string_of_interval = function
		Empty -> "empty"
  |	Interval (a,b) -> "["+(string_of_int a)+","+(string_of_int b)+"]";;

let print_interval = print_string string_of_interval;;

(*** Pointwise operations ***)

let x_plus (x,y,z) = (plus_one x),y,z;;
let y_plus (x,y,z) = x,(plus_one y),z;;
let z_plus (x,y,z) = x,y,(plus_one z);;
let x_minus (x,y,z) = (minus_one x),y,z;;
let y_minus (x,y,z) = x,(minus_one y),z;;
let z_minus (x,y,z) = x,y,(minus_one z);;
let x_zero (x,y,z) = (zero x),y,z;;
let y_zero (x,y,z) = x,(zero y),z;;
let z_zero (x,y,z) = x,y,(zero z);;
let x_non_zero (x,y,z) = (non_zero x),y,z;;
let y_non_zero (x,y,z) = x,(non_zero y),z;;
let z_non_zero (x,y,z) = x,y,(non_zero z);;
