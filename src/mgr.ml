open Urm

type expression =
	| S of expression * expression
	| I of int


(********************************)
(*          EXERCICE 1          *)
(********************************)

let succ = [|Incr(0)|];;
let somme = [|Incr(0); Incr(2); Jump(1, 2, max_step - 1); Jump(0, 0, 0)|];;
let constant = empty;;
let bigger = [|Set(0, 2); Set(1, 3); Jump(0, 3, 10); Jump(1, 2, 7); Incr(0);
               Incr(1); Jump(0, 0, 3); Reset(0); Incr(0);
               Jump(0, 0, max_step - 1); Reset(0)
             |];;


(********************************)
(*          EXERCICE 2          *)
(********************************)

let string_of_prog prog =
	print_prog prog

let debug_program reg_array prog =
	let rho = rho prog in
	let _ = print_reg_array reg_array rho in
	let _ = run_program reg_array prog in
	print_reg_array reg_array rho


(********************************)
(*          EXERCICE 3          *)
(********************************)

let compose1 prog1 prog2 =
	let l = Array.length prog1 in
	let _ = Array.map (
		fun inst ->
			match inst with
			| Jump(n1, n2, n3) -> Jump(n1, n2, l + n3)
			| _ -> inst
		) prog2
	in
	Array.append prog1 prog2


(********************************)
(*          EXERCICE 4          *)
(********************************)

let array_vect len offset =
	Array.init len (fun i -> i + offset)

let translate prog reg_vect r =
	let rho_prog = rho prog in
	let len_reg_vect = Array.length reg_vect in
	let prog1 = Array.init len_reg_vect (
		fun i ->
			let src = reg_vect.(i) in
			Set(src, i)
		)
	in
	let prog2 =
		try Array.init (rho_prog - len_reg_vect) (
			fun i -> Reset(i + len_reg_vect)
			)
		with Invalid_argument _ -> empty
	in
	let ret1 = Array.append prog1 prog2 in
	let ret2 = compose1 ret1 prog in
	Array.append ret2 [|Set(0, r)|]


(********************************)
(*          EXERCICE 5          *)
(********************************)

let compose2 progF progG_vect k =
	let max_rhoG = Array.fold_left (
		fun max_rho prog -> max max_rho (rho prog)
		) 0 progG_vect
	in
	let regN = max (max (Array.length progF) k) (max (rho progF) max_rhoG) in
	let reg_vect = array_vect k regN in
	let reg_vect_k = array_vect k (regN + k) in
	let progG_array = Array.mapi (
		fun i prog -> translate prog reg_vect (regN + k + i)
		) progG_vect
	in
	let progF_ret = translate progF reg_vect_k 0 in
	let ret = Array.fold_left (
		fun prog1 prog2 -> compose1 prog1 prog2
		) empty progG_array
	in
	compose1 ret progF_ret


(********************************)
(*          EXERCICE 6          *)
(********************************)

let prog_of_expr expr =
	let rho_somme = rho somme in
	let rec aux expr i =
		match expr with
		| S(expr1, expr2) ->
			let prog1 = aux expr1 (i + rho_somme) in
			let prog2 = aux expr2 (i + rho_somme) in
			let rho1 = rho prog1 in
			let rho2 = rho prog2 in
			let reg_vect1 = array_vect rho1 i in
			let reg_vect2 = array_vect rho2 (i + rho1) in
			let prog1_t = translate prog1 reg_vect1 i in
			let prog2_t = translate prog1 reg_vect2 (i + rho1) in
			compose1 prog1_t prog2_t
		| I(n) ->
			let reg_vect = array_vect 1 i in
			translate constant reg_vect i
	in
	aux expr 0


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
	let progT_t = translate progT reg_vect_T 0 in
	let progF_t = translate progF reg_vect_F 0 in
	let ret1 = compose1 [|Reset(rhoC)|] progC in
	let ret2 = compose1 ret1 [|Jump(0, rhoC, lenC + lenT - 1)|] in
	let ret3 = compose1 ret2 progT_t in
	let ret4 = compose1 ret3 [|Jump(0, 0, max_step)|] in
	compose1 ret4 progF_t
