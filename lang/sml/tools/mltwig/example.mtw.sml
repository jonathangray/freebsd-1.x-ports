structure TreeProcessor =
struct
  structure User =
  struct
datatype symbol =
ARC of int | XXXExpr | Const | Mul | Minus | Plus

   
   (* The type and function definitions                   *)

      datatype tree = Tree of (tree * symbol * tree) | Leaf of int
      type cost = int

      fun get_subtrees(Leaf _) = []
        | get_subtrees(Tree (t1,_,t2)) = [t1,t2]
      fun node_value(Tree(_,ope,_))  = ope
        | node_value (Leaf _) = Const
      val cost_less : int * int -> bool = (op <)

      fun constValue (Leaf i) = i
      datatype instr = PUSH of int | PLUS | MINUS | MUL | PLUSMUL
   

datatype result = XXXrewrite of tree | X_Expr of  instr list
 end

structure Specification =
  struct
structure User = User

open User
type rule = int
datatype skeletal = Skeleton of (rule * cost * tree * skeletal list)
exception MatchAbort
fun cost (Skeleton(_,c,_,_)) = c
exception InternalError of string

fun get_subtree (n,t) = nth (get_subtrees t,n-1)

fun execute_cost (n:rule, ir, children) =
let open User
val DC = (  fn subexprcost => fold (op +) subexprcost 0) (map cost children)
val ABORT = (fn () => raise MatchAbort)
 in
case n of
  5 => (case map cost children of [Expr1,Expr2,Expr3] => (( 0 )) | _ => raise InternalError "S4")
  | 4 => (case map cost children of [Expr1,Expr2,Expr3] => (( 3+DC )) | _ => raise InternalError "S4")
  | 3 => (case map cost children of [Expr1,Expr2] => (( 2+DC )) | _ => raise InternalError "S4")
  | 2 => (case map cost children of [Expr1,Expr2] => (( 2+Expr1+Expr2 )) | _ => raise InternalError "S4")
  | 1 => (case map cost children of [Expr1,Expr2] => (( 2+Expr1+Expr2 )) | _ => raise InternalError "S4")
  | 0 => DC
  | _ => raise InternalError "S4.3."
end

fun execute (Skeleton (n,_, ir, children)) =
 let open User
in
case n of
5 => XXXrewrite ( case (()) of (_) => ( Tree((get_subtree (2,ir)),Mul,(get_subtree (1,ir)) ) ) )
  | 4 => X_Expr ( case (execute (nth(children,0)),execute (nth(children,1)),execute (nth(children,2))) of (X_Expr Expr1,X_Expr Expr2,X_Expr Expr3) => ( Expr1@Expr2@Expr3@[PLUSMUL] ) | _ => raise InternalError "S5" )
  | 3 => X_Expr ( case (execute (nth(children,0)),execute (nth(children,1))) of (X_Expr Expr1,X_Expr Expr2) => ( Expr1@Expr2@[MUL] ) | _ => raise InternalError "S5" )
  | 2 => X_Expr ( case (execute (nth(children,0)),execute (nth(children,1))) of (X_Expr Expr1,X_Expr Expr2) => ( Expr1@Expr2@[MINUS] ) | _ => raise InternalError "S5" )
  | 1 => X_Expr ( case (execute (nth(children,0)),execute (nth(children,1))) of (X_Expr Expr1,X_Expr Expr2) => ( Expr1@Expr2@[PLUS] ) | _ => raise InternalError "S5" )
  | 0 => X_Expr( [PUSH (constValue ir )] )
  | _ => raise Match
end

val matchcounts = [
(5,3),
(4,3),
(3,2),
(2,2),
(1,2),
(0,1)]
val matchtable = let val a = Array.array(6,0) in ((app (fn(r,m)=>Array.update (a,r,m)) matchcounts); a) end

fun matches r = Array.sub(matchtable, r)

fun get_finals s =
  case s of
0 => []
  | 1 => []
  | 2 => []
  | 3 => [(2,1,XXXExpr)]
  | 4 => []
  | 5 => [(2,1,XXXExpr)]
  | 6 => []
  | 7 => []
  | 8 => [(2,2,XXXExpr)]
  | 9 => []
  | 10 => [(2,2,XXXExpr)]
  | 11 => []
  | 12 => []
  | 13 => [(2,4,XXXExpr),(2,3,XXXExpr)]
  | 14 => []
  | 15 => [(2,5,XXXExpr),(2,3,XXXExpr)]
  | 16 => []
  | 17 => []
  | 18 => [(2,1,XXXExpr),(3,4,XXXExpr)]
  | 19 => []
  | 20 => [(2,1,XXXExpr),(3,4,XXXExpr)]
  | 21 => []
  | 22 => []
  | 23 => [(2,1,XXXExpr),(3,5,XXXExpr)]
  | 24 => []
  | 25 => [(2,1,XXXExpr),(3,5,XXXExpr)]
  | _ => nil

fun go (s,a) =
  case s of
0 => (case a of Mul => 11 | Minus => 6 | Plus => 1 |  _ => 0)
  | 1 => (case a of (ARC 2) => 4 | (ARC 1) => 2 |  _ => go (0,a))
  | 2 => (case a of XXXExpr => 3 |  _ => go (0,a))
  | 3 => (case a of  _ => go (0,a))
  | 4 => (case a of XXXExpr => 5 |  _ => go (0,a))
  | 5 => (case a of  _ => go (0,a))
  | 6 => (case a of (ARC 2) => 9 | (ARC 1) => 7 |  _ => go (0,a))
  | 7 => (case a of XXXExpr => 8 |  _ => go (0,a))
  | 8 => (case a of  _ => go (0,a))
  | 9 => (case a of XXXExpr => 10 |  _ => go (0,a))
  | 10 => (case a of  _ => go (0,a))
  | 11 => (case a of (ARC 2) => 14 | (ARC 1) => 12 |  _ => go (0,a))
  | 12 => (case a of Plus => 21 | XXXExpr => 13 |  _ => go (0,a))
  | 13 => (case a of  _ => go (0,a))
  | 14 => (case a of Plus => 16 | XXXExpr => 15 |  _ => go (0,a))
  | 15 => (case a of  _ => go (0,a))
  | 16 => (case a of (ARC 2) => 19 | (ARC 1) => 17 |  _ => go (1,a))
  | 17 => (case a of XXXExpr => 18 |  _ => go (2,a))
  | 18 => (case a of  _ => go (3,a))
  | 19 => (case a of XXXExpr => 20 |  _ => go (4,a))
  | 20 => (case a of  _ => go (5,a))
  | 21 => (case a of (ARC 2) => 24 | (ARC 1) => 22 |  _ => go (1,a))
  | 22 => (case a of XXXExpr => 23 |  _ => go (2,a))
  | 23 => (case a of  _ => go (3,a))
  | 24 => (case a of XXXExpr => 25 |  _ => go (4,a))
  | 25 => (case a of  _ => go (5,a))
  | _ => 0

val go_f = get_finals o go
fun childsymbol s = ARC s
val initialstate = 0
type state = int
datatype matchtree = Chain of int * symbol * matchtree list
fun unitmatches nt = (case nt of
Const => [Chain (0,XXXExpr,[])]
  | _ => [])

fun rewriterule (r:rule) =
 case r of
5 => true |_ => false
fun getreplacement (XXXrewrite t) = t | getreplacement _ = raise InternalError "problem with rewrite 996"
  end
structure Internal = MAKEtreeprocessor(Specification)
exception NoCover = Internal.NoCover
exception InternalError = Internal.InternalError
val translate = Internal.translate
end;
