type instruction =
	| Reset of int
	| Incr of int
	| Set of int * int
	| Jump of int * int * int

type prog = instruction array
type reg_array = int array

let max_reg = 8
let max_steps = 100

exception Memory_exhausted
exception Segmentation_fault
exception Resources_exhausted

let empty = Array.make 0 (Set(0,0))

let is_memory_exhausted n =
	if compare n max_reg >= 0 then
	raise Memory_exhausted
	else
	()

let is_segmentation_fault n =
	if compare n max_reg >= 0 then
	raise Segmentation_fault
	else
	()

let is_resources_exhausted steps =
	if steps = 0 then
	raise Resources_exhausted
	else
	()

let print_reg_array reg_array rho =
	let _ = Array.iter (
		fun n ->
			Printf.printf "|\t%d" n
		) reg_array
	in
	Printf.printf "\n"

let print_prog prog =
	Array.iter (
		fun inst ->
			match inst with
			| Reset(n1) -> Printf.printf "Reset(%d)\n" n1
			| Incr(n1) -> Printf.printf "Incr(%d)\n" n1
			| Set(n1, n2) -> Printf.printf "Set(%d,%d)\n" n1 n2
			| Jump(n1, n2, n3) -> Printf.printf "Jump(%d,%d,%d)\n" n1 n2 n3
		) prog

let run_instruction reg_array inst i =
	match inst with
	| Reset(n1) ->
		let _ = is_memory_exhausted n1 in
		let _ = is_segmentation_fault n1 in
		reg_array.(n1) <- 0;
		i + 1
	| Incr(n1) ->
		let _ = is_memory_exhausted n1 in
		let _ = is_segmentation_fault n1 in
		reg_array.(n1) <- reg_array.(n1) + 1;
		i + 1
	| Set(n1, n2) ->
		let _ = is_memory_exhausted n1 in
		let _ = is_segmentation_fault n2 in
		reg_array.(n2) <- reg_array.(n1);
		i + 1
	| Jump(n1, n2, n3) ->
		let _ = is_memory_exhausted n1 in
		let _ = is_memory_exhausted n2 in
		if n1 = n2 then
			n3
		else
			i + 1

let rho prog =
	Array.fold_left (
		fun reg_max inst ->
			match inst with
			| Reset(n1) -> max reg_max n1
			| Incr(n1) -> max reg_max n1
			| Set(n1, _) -> max reg_max n1
			| _ -> reg_max
		) 0 prog

let normalize prog =
	let n = Array.length prog in
	Array.map (
		fun inst ->
			match inst with
			| Jump(n1, n2, _) -> Jump(n1, n2, n)
			| _ -> inst
		) prog

let clean_program prog =
	let reg_max = rho prog in
	let reset_prog = Array.init reg_max (
		fun n -> Reset(n + 1)
		)
	in
	Array.append prog reset_prog

let step_program reg_array prog max_steps =
	let rec aux i steps =
		let _ = is_resources_exhausted steps in
		let next_i = run_instruction reg_array (prog.(i)) i in
		if next_i >= (Array.length prog) then
			reg_array.(0)
		else
			aux next_i (steps - 1)
	in
	aux 0 max_steps

let run_program reg_array prog =
	let _ = step_program reg_array prog max_steps in
	reg_array.(0)

let run_function prog n =
	let reg_array = Array.make max_reg 0 in
	reg_array.(0) <- n;
	let _ = run_program reg_array prog in
	let rho = rho prog in
	print_reg_array reg_array rho
