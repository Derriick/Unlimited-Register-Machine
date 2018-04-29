type instruction =
	| Reset of int
	| Incr of int
	| Set of int * int
	| Jump of int * int * int

type program = instruction array
type reg_array = int array

let max_reg = 128
let max_steps = 200

exception Memory_exhausted
exception Segmentation_fault
exception Resources_exhausted

let empty = Array.make 0 (Set(0,0))

let is_memory_exhausted n =
	if n >= max_reg then
	raise Memory_exhausted
	else
	()

let is_segmentation_fault n =
	if n >= max_reg then
	raise Segmentation_fault
	else
	()

let is_resources_exhausted steps =
	if steps = 0 then
	raise Resources_exhausted
	else
	()

let print_reg_array reg_array max =
	for i = 0 to max do
		Printf.printf "| %d\t" reg_array.(i)
	done;
	Printf.printf "|\n"

let print_prog prog =
	Array.iteri (
		fun i inst ->
			match inst with
			| Reset(n1) -> Printf.printf "%8d Reset(%d)\n" i n1
			| Incr(n1) -> Printf.printf "%8d Incr(%d)\n" i n1
			| Set(n1, n2) -> Printf.printf "%8d Set(%d,%d)\n" i n1 n2
			| Jump(n1, n2, n3) -> Printf.printf "%8d Jump(%d,%d,%d)\n" i n1 n2 n3
		) prog

let run_instruction reg_array inst i =
	reg_array.(0) <- i;
	match inst with
	| Reset(n) ->
		let _ = is_memory_exhausted n in
		let _ = is_segmentation_fault n in
		reg_array.(n) <- 0;
		i + 1
	| Incr(n) ->
		let _ = is_memory_exhausted n in
		let _ = is_segmentation_fault n in
		reg_array.(n) <- reg_array.(n) + 1;
		i + 1
	| Set(n1, n2) ->
		let _ = is_memory_exhausted n1 in
		let _ = is_segmentation_fault n2 in
		reg_array.(n2) <- reg_array.(n1);
		i + 1
	| Jump(n1, n2, n3) ->
		let _ = is_memory_exhausted n1 in
		let _ = is_memory_exhausted n2 in
		if reg_array.(n1) = reg_array.(n2) then
			n3
		else
			i + 1

let rho prog =
	Array.fold_left (
		fun reg_max inst ->
			match inst with
			| Reset(r) -> max reg_max r
			| Incr(r) -> max reg_max r
			| Set(r1, r2) -> max reg_max (max r1 r2)
			| Jump(r1, r2, _) -> max reg_max (max r1 r2)
		) 0 prog

let normalize prog =
	let n = Array.length prog in
	Array.map (
		fun inst ->
			match inst with
			| Jump(n1, n2, _) -> Jump(n1, n2, n) (* n = nombre d'instructions + 1 *)
			| _ -> inst
		) prog

let clean_program prog =
	let rho_prog = rho prog in
	let reset_prog = Array.init (rho_prog - 2) (
		fun n -> Reset(n + 2)
		)
	in
	Array.append prog reset_prog

let step_program reg_array prog max_steps =
	let rec aux i steps =
		let _ = is_resources_exhausted steps in
		let next_i = run_instruction reg_array (prog.(i)) i in
		if next_i >= (Array.length prog) then
			reg_array.(1)
		else
			aux next_i (steps - 1)
	in
	aux 0 max_steps

let run_program reg_array prog =
	step_program reg_array prog max_steps

let run_function prog n =
	let reg_array = Array.make max_reg 0 in
	reg_array.(1) <- n;
	let _ = run_program reg_array prog in
	let rho = rho prog in
	print_reg_array reg_array rho
