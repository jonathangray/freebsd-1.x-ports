(*
% ForML Version 0.6 - 25 January 1993 - er@cs.cmu.edu
*)
open Formatter;
Pagewidth := 20; (* setting output width to 20 characters *)

(**********************************)
(* \lambda-calculus like language *)
(**********************************)

(*** Abstract syntax trees ***)
datatype exp =
         Var of string
      |  Lam of string * exp
      |  App of exp * exp
      |  Let of string * exp * exp

(* the expression  "\x. \y. \z. let x=y in y (x y) (z y)"  *)
val E1 = Lam("x", Lam("y", Lam("z",
             Let("x",Var "y",
                   App(App(Var "y", App(Var "x",Var"y")),
                       App(Var"z", Var"y"))))))


(*** formatter for \lambda-calculus like language ***)

fun format (Let(ID,exp1,exp2)) =
           HOVbox[String "let", Space,
                  HOVbox[String ID, String "=", Break, format exp1],
                  Break, String "in", Space, format exp2]
  | format (Lam(ID,exp)) = 
           HOVbox[ String "\\", String ID, String ".", Break, format exp]
  | format exp = format_A exp
and format_A (App(expa,expb)) = HOVbox[ format_A expa, Break, format_B expb ]
  | format_A exp = format_B exp
and format_B (Var ID) = String ID
  | format_B exp = HOVbox[ String "(", format exp, String ")" ]
;

(* output from format *)
(print "\n"; print_fmt(format E1); print "\n");


(*** formatter for \lambda-calculus like language, second version ***)

fun format' (Let(ID,exp1,exp2)) =
           HOVbox[String "let", Space,
                  HOVbox[String ID, String "=", Break0 0 3, format' exp1],
                  Break0 1 1, String "in", Space, format' exp2]
  | format' (a as Lam(ID,exp)) = format_abs' a nil
  | format' exp = format_A' exp
and format_abs' (Lam(ID,exp)) absl = 
                format_abs' exp (absl @ [String("\\" ^ ID ^ "."), Break])
  | format_abs' exp absl = HVbox(absl @ [format' exp])
and format_A' (App(expa,expb)) = HOVbox[ format_A' expa, Break, format_B' expb ]
  | format_A' exp = format_B' exp
and format_B' (Var ID) = String ID
  | format_B' exp = HOVbox[ String "(", format' exp, String ")" ]
;
(* output from format' *)
(print "\n"; print_fmt(format' E1); print "\n");


(*************************)
(* Mini-ML like language *)
(*************************)

(*** Abstract syntax trees ***)

datatype exp = Var of string | Int of int | Bool of bool
                  | App of exp * exp 
                  | Lam of string * exp
                  | Let of string * exp * exp 
                  | If of exp * exp * exp
                  | List of exp list
                  | Op of string * int * exp * exp

(* A Mini-ML expression *)
local
  val l = List[ Int 1, Int ~1, Int 0, Bool true, Bool false,
                Lam("x",Op("+",5,Var"x", Int 1)) ]
  val f = Lam("x", Lam("y", Lam("z",
             If(Op(">",1,Op("+",5,Var"y",Var"z"),Int 0), Var"x",Var"y"))))
  val e = Op("-",5,
             Op("+",5,
                Int 1,
                Op("/",10,
                   Op("*",10,
                      Op("+",5,Int 2,Var "x"),
                      Int 3),
                   Op("-",5,Int 4,Int 5))),
             Op("+",5,
                Op("*",10,Int 6, Int 7),
                Int 8))
in
  val E2 = Lam("x", Lam("y", Lam("z",
               Let("silly", f, App(App(App(Var "silly",l),e),Int 15)))))
end


(*** Formatter for Mini-ML expressions ***)

(* Auxiliary function *)
infixr 5 @@

fun (l1 @@ nil) = nil
  | (l1 @@ l2) = l1 @ l2

fun mformat (Var s) = String s
  | mformat (Int n) = String (makestring n)
  | mformat (Bool b) = String (if b then "true" else "false")
  | mformat (e as Lam _) = mformat_lam e []
  | mformat (e as App (e1,e2)) = mformat_app e1 [mformat_op 50 e2]
  | mformat (Let(s,e1,e2)) =
            HOVbox[String "let", Space, HOVbox[String s, String "=", Break0 3 1,
                                               mformat e1], Break0 1 1,
                   String "in", Space, mformat e2]
  | mformat (If(e1,e2,e3)) = HOVbox[ String "if", Space, mformat e1, Break,
                                     String "then", Space, mformat e2, Break,
                                     String "else", Space, mformat e3]
  | mformat (List l) =
            Hbox[ String "[",
                  HVbox (fold (fn (el, fmtl) =>
                                  [ mformat el ] @
                                        [String ",", Break0 1 0] @@ fmtl)
                              l
                              nil),
                  String "]" ]
  | mformat (e as Op(_,p,_,_)) = mformat_op p e
and mformat_lam (Lam(s,e)) l = 
                mformat_lam e (l @ [String("\\" ^ s ^ "."), Break])
  | mformat_lam e l = HVbox(l @ [mformat e])
and mformat_app (App(e1,e2)) l = mformat_app e1 ((mformat_op 50 e2)::Break::l)
  | mformat_app e l = HVbox((mformat_op 50 e)::Break::l)
and mformat_op p' (Op(s,p,e1,e2)) =
    HOVbox( (if p'>p then [String "("] else [])
            @ [mformat_op p e1] @ [ Break0 0 1, String s]
            @ [mformat_op p e2] @ 
            (if p'>p then [String ")"] else []) )
  | mformat_op p e = mformat e
;

(* output sample expression *)
(print "\n"; print_fmt(mformat E2); print "\n");
