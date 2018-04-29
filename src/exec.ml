open Urm
open Mgr

let print_int n = Printf.printf "%d\n\n" n

let exec_prog reg_array prog =
	let _ = string_of_prog prog in
	debug_program reg_array prog
	(*run_program reg_array prog*)

let exec1 prog n =
	let reg_array = Array.make max_reg 0 in
	reg_array.(1) <- n;
	print_int (exec_prog reg_array prog)

let exec2 prog n1 n2 =
	let reg_array = Array.make max_reg 0 in
	reg_array.(1) <- n1;
	reg_array.(2) <- n2;
	print_int (exec_prog reg_array prog)

let exec3 prog n1 n2 n3 =
	let reg_array = Array.make max_reg 0 in
	reg_array.(1) <- n1;
	reg_array.(2) <- n2;
	reg_array.(3) <- n3;
	print_int (exec_prog reg_array prog)

let exec4 prog n1 n2 n3 n4 =
	let reg_array = Array.make max_reg 0 in
	reg_array.(1) <- n1;
	reg_array.(2) <- n2;
	reg_array.(3) <- n3;
	reg_array.(4) <- n4;
	print_int (exec_prog reg_array prog)
