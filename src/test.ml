open Urm
open Mgr

let exec_succ n =
	let _ = string_of_prog succ in
	let reg_array = Array.make max_reg 0 in
	reg_array.(0) <- n;
	run_program reg_array succ

let exec_somme n1 n2 =
	let _ = string_of_prog constant in
	let reg_array = Array.make max_reg 0 in
	reg_array.(0) <- n1;
	reg_array.(1) <- n2;
	run_program reg_array somme

let exec_constant n =
	let _ = string_of_prog constant in
	let reg_array = Array.make max_reg 0 in
	reg_array.(0) <- n;
	run_program reg_array constant

let exec_bigger n1 n2 =
	let _ = string_of_prog constant in
	let reg_array = Array.make max_reg 0 in
	reg_array.(0) <- n1;
	reg_array.(1) <- n2;
	run_program reg_array bigger
