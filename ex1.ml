open Prog

let succ n =
   let reg_array = Array.make max_reg 0 in
   reg_array.(0) <- n;
   let prog = [|Incr(0)|] in
   run_program reg_array prog

let somme n1 n2 =
   let reg_array = Array.make max_reg 0 in
   reg_array.(0) <- n1;
   reg_array.(1) <- n2;
   let prog = [|Incr(0); Incr(2); Jump(1, 2, 4); Jump(0, 0, 0)|] in
   run_program reg_array prog

let constant n =
   let reg_array = Array.make max_reg 0 in
   reg_array.(0) <- n;
   let prog = [||] in
   run_program reg_array prog
