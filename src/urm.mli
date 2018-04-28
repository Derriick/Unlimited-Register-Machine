type instruction =
	| Reset of int
	| Incr of int
	| Set of int * int
	| Jump of int * int * int

type prog = instruction array
type reg_array = int array

exception Memory_exhausted
exception Segmentation_fault
exception Resources_exhausted

val max_reg : int
val max_steps : int
val empty : prog

val print_reg_array : reg_array -> int -> unit
val print_prog : prog -> unit
val rho : prog -> int
val normalize : prog -> prog
val run_program : reg_array -> prog -> int
