type instruction =
	| Reset of int
	| Incr of int
	| Set of int * int
	| Jump of int * int * int

type program = instruction array
type reg_array = int array

exception Memory_exhausted
exception Segmentation_fault
exception Resources_exhausted

val max_reg : int
val max_steps : int
val empty : program

val print_reg_array : reg_array -> int -> unit
val print_prog : program -> unit
val rho : program -> int
val normalize : program -> program
val clean_program : program -> program
val run_program : reg_array -> program -> int
