open Prog

let string_of_prog prog =
	print_prog prog

let debug_program reg_array prog =
   let rho = rho prog in
   let _ = print_reg_array reg_array rho in
   let _ = run_program reg_array prog in
   print_reg_array reg_array rho
