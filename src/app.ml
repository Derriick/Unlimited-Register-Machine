open Mgr
open Exec

let a = 8
let b = 11

(********************************)
(*          EXERCICE 1          *)
(********************************)
let _ = exec1 succ a
let _ = exec2 somme a b
let _ = exec1 constant a
let _ = exec2 bigger a b

(********************************)
(*          EXERCICE 2          *)
(********************************)
let _ = exec2 (compose1 succ somme) a b
let _ = exec2 (compose1 somme bigger) a b
let _ = exec2 (compose1 (compose1 succ constant) bigger) a b
let _ = exec2 (compose1 succ (compose1 constant bigger)) a b (* équivalent à celui d'au-dessus *)
let _ = exec2 (compose1 (compose1 succ (compose1 succ (compose1 succ (compose1 succ succ)))) bigger) a b
let _ = exec2 (compose1 (compose1 somme somme) (compose1 succ bigger)) a b

(********************************)
(*         EXERCICES 4-5        *)
(********************************)
let _ = exec1 (compose2 somme [|succ; succ|] 1) a
let _ = exec2 (compose2 somme [|somme; bigger|] 2) b a
let _ = exec2 (compose2 somme [|compose1 succ (compose1 (compose1 succ (compose1 succ succ)) bigger); bigger|] 2) a b

(********************************)
(*          EXERCICE 6          *)
(********************************)


(********************************)
(*          EXERCICE 7          *)
(********************************)
let _ = exec2 (if_then_else bigger somme succ) a b
let _ = exec2 (if_then_else bigger somme succ) b a
