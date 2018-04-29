open Urm

type expression =
	| S of expression * expression
	| I of int

(*******************************)
(*          EXERCICE 1         *)
(*******************************)
val succ : program
val somme : program
val constant : program
val bigger : program

(*******************************)
(*          EXERCICE 2         *)
(*******************************)
val string_of_prog : program -> unit
val debug_program : reg_array -> program -> unit

(*******************************)
(*          EXERCICE 3         *)
(*******************************)
val compose1 : program -> program -> program

(*******************************)
(*          EXERCICE 4         *)
(*******************************)
val translate : program  -> reg_array -> int -> program

(*******************************)
(*          EXERCICE 5         *)
(*******************************)
val compose2 : program -> program array -> int -> program

(*******************************)
(*          EXERCICE 6  prog_of_expr       *)
(*******************************)
val prog_of_expr : expression -> program

(*******************************)
(*          EXERCICE 7         *)
(*******************************)
val if_then_else : program -> program -> program -> program
