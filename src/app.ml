open Mgr
open Exec

let a = 5
let b = 7
let c = 11
let d = 13

(********************************)
(*          EXERCICE 1          *)
(********************************)
let _ = exec1 succ a
let _ = exec2 sum a b
let _ = exec1 constant a
let _ = exec2 bigger a b

(********************************)
(*          EXERCICE 2          *)
(********************************)
let _ = exec2 (compose1 succ sum) a b
let _ = exec2 (compose1 sum bigger) a b
let _ = exec2 (compose1 (compose1 succ constant) bigger) a b
let _ = exec2 (compose1 succ (compose1 constant bigger)) a b (* équivalent à celui d'au-dessus *)
let _ = exec2 (compose1 (compose1 succ (compose1 succ (compose1 succ (compose1 succ succ)))) bigger) a b
let _ = exec2 (compose1 (compose1 sum sum) (compose1 succ bigger)) a b

(********************************)
(*         EXERCICES 4-5        *)
(********************************)
let _ = exec1 (compose2 sum [|succ; succ|] 1) a
let _ = exec2 (compose2 sum [|sum; bigger|] 2) b a
let _ = exec2 (compose2 sum [|compose1 succ (compose1 (compose1 succ (compose1 succ succ)) bigger); bigger|] 2) a b

(********************************)
(*          EXERCICE 6          *)
(********************************)
let e1 = S(I(1), I(2))
let e2 = S(I(1), S(I(3), I(2)))
let e3 = S(I(4), S(I(3), S(I(1), I(2))))
let _ = exec2 (prog_of_expr e1) a b
let _ = exec3 (prog_of_expr e2) a b c
let _ = exec4 (prog_of_expr e3) a b c d

(********************************)
(*          EXERCICE 7          *)
(********************************)
let _ = exec1 (if_then_else constant succ constant) 0
let _ = exec1 (if_then_else constant succ constant) 1
let _ = exec2 (if_then_else bigger sum succ) a b
let _ = exec2 (if_then_else bigger sum succ) b a
