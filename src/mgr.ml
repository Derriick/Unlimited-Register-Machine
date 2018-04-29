open Urm

type expression =
	| S of expression * expression
	| I of int


(********************************)
(*          EXERCICE 1          *)
(********************************)

let succ = [|Incr(1)|]
let somme = [|Reset(3); Jump(2, 3, 5); Incr(1); Incr(3); Jump(0, 0, 1)|]
let constant = [|Set(1, 1)|]
let bigger = [|Set(1, 3); Set(2, 4); Jump(1, 4, 10); Jump(2, 3, 7); Incr(1); Incr(2);
               Jump(0, 0, 2); Reset(1); Incr(1); Jump(0, 0, 11); Reset(1)|]


(********************************)
(*          EXERCICE 2          *)
(********************************)

let string_of_prog prog =
	print_prog prog

let debug_program reg_array prog =
	let rho = rho prog in
	let _ = print_reg_array reg_array rho in
	let _ = run_program reg_array prog in
	let _ = print_reg_array reg_array rho in
	reg_array.(1)


(********************************)
(*          EXERCICE 3          *)
(********************************)

let compose1 prog1 prog2 =
	let len = Array.length prog1 in
	let prog1' = Array.map (
		fun inst ->
			match inst with
			| Jump(n1, n2, n3) -> Jump(n1, n2, min n3 len)
			| _ -> inst
		) prog1
	in
	let prog2' = Array.map (
		fun inst ->
			match inst with
			| Jump(n1, n2, n3) -> Jump(n1, n2, n3 + len)
			| _ -> inst
		) prog2
	in
	Array.append prog1' prog2'


(********************************)
(*          EXERCICE 4          *)
(********************************)

let translate prog reg_vect r =
	let rho_prog = rho prog in
	let len_reg_vect = Array.length reg_vect in
	let prog1 = Array.init len_reg_vect (
		fun i ->
			let src = reg_vect.(i) in
			Set(src, i + 1)
		)
	in
	let prog2 =
		try Array.init (rho_prog - len_reg_vect - 1) (
			fun i -> Reset(i + len_reg_vect + 1)
			)
		with Invalid_argument _ -> empty
	in
	let ret1 = Array.append prog1 prog2 in
	let ret2 = compose1 ret1 prog in
	Array.append ret2 [|Set(1, r)|]

let array_vect len offset =
	Array.init len (fun i -> i + offset)


(********************************)
(*          EXERCICE 5          *)
(********************************)

let compose2 progF progG_vect k =
	let max_rhoG = Array.fold_left (
		fun max_rho prog -> max max_rho (rho prog)
		) 0 progG_vect
	in
	let n = Array.length progG_vect in
	let regN = max (max (Array.length progF) k) (max (rho progF) max_rhoG) in
	let prog_save = Array.init k (
		fun i -> Set(i + 1, regN + i)
		)
	in
	let reg_vect = array_vect k regN in
	let reg_vect_F = array_vect n (regN + k) in
	let progG_array = Array.mapi (
		fun i prog -> translate prog reg_vect (regN + k + i)
		) progG_vect
	in
	let progF_ret = translate progF reg_vect_F 1 in
	let ret1 = Array.fold_left (
		fun prog1 prog2 -> compose1 prog1 prog2
		) empty progG_array
	in
	let ret2 = compose1 prog_save ret1 in
	compose1 ret2 progF_ret


(********************************)
(*          EXERCICE 6          *)
(********************************)

let prog_of_expr expr =
	let rho_somme = rho somme in
	let rec aux expr =
		match expr with
		| S(expr1, expr2) ->
			let prog1 = aux expr1 in
			let prog2 = aux expr2 in
			let rho1 = rho prog1 in
			let rho2 = rho prog2 in
			let reg_vect2 = array_vect rho2 (rho1 + 1) in
			let prog2_t = translate prog2 reg_vect2 (rho1 + 1) in
			let prog = compose1 prog1 prog2_t in
			let reg_vect = array_vect rho_somme (rho_somme + rho1 + rho2 + 2) in
			let prog_t = translate prog reg_vect (rho1 + rho2 + 2) in
			compose1 prog_t somme
		| I(n) -> constant
	in
	aux expr


(********************************)
(*          EXERCICE 7          *)
(********************************)

let if_then_else progC progT progF =
	let lenC = Array.length progC in
	let lenT = Array.length progT in
	let rhoC = rho progC in
	let rhoT = rho progT in
	let rhoF = rho progF in
	let reg_vect_T = array_vect rhoT (rhoC + 1) in
	let reg_vect_F = array_vect rhoF (rhoC + rhoT + 1) in
	let progT_t = translate progT reg_vect_T 1 in
	let progF_t = translate progF reg_vect_F 1 in
	let ret1 = compose1 [|Reset(rhoC)|] progC in
	let ret2 = compose1 ret1 [|Jump(1, rhoC, lenC + lenT - 1)|] in
	let ret3 = compose1 ret2 progT_t in
	let ret4 = compose1 ret3 [|Jump(0, 0, max_steps)|] in
	compose1 ret4 progF_t
