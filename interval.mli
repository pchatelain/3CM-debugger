(* Inteval domain *)

type interval = Empty | Interval of int * int

(* Bottom constant *)

val bottom : unit -> interval

(* Unary operations *)

val zero : interval -> interval
val non_zero : interval -> interval
val plus_one : interval -> interval
val minus_one : interval -> interval

(* Binary operations *)

val smaller : interval -> interval -> bool
val union : interval -> interval -> interval

(* Pretty printing *)

val string_of_interval : interval -> string
val print_interval : interval -> unit

(* Pointwise operations *)

val x_plus : (interval * interval * interval) -> (interval * interval * interval)
val y_plus : (interval * interval * interval) -> (interval * interval * interval)
val z_plus : (interval * interval * interval) -> (interval * interval * interval)
val x_minus : (interval * interval * interval) -> (interval * interval * interval)
val y_minus : (interval * interval * interval) -> (interval * interval * interval)
val z_minus : (interval * interval * interval) -> (interval * interval * interval)
val x_zero : (interval * interval * interval) -> (interval * interval * interval)
val y_zero : (interval * interval * interval) -> (interval * interval * interval)
val z_zero : (interval * interval * interval) -> (interval * interval * interval)
val x_non_zero : (interval * interval * interval) -> (interval * interval * interval)
val y_non_zero : (interval * interval * interval) -> (interval * interval * interval)
val z_non_zero : (interval * interval * interval) -> (interval * interval * interval)
