(**** ML Programs from the book

  ML for the Working Programmer
  by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
  (Cambridge University Press, 1991)

Copyright (C) 1991 by Cambridge University Press.
Permission to copy without fee is granted provided that this copyright
notice and the DISCLAIMER OF WARRANTY are included in any copy.

DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
warranty of any kind.  We make no warranties, express or implied, that the
programs are free of error, or are consistent with any particular standard
of merchantability, or that they will meet your requirements for any
particular application.  They should not be relied upon for solving a
problem whose incorrect solution could result in injury to a person or loss
of property.  If you do use the programs or functions in such a manner, it
is at your own risk.  The author and publisher disclaim all liability for
direct, incidental or consequential damages resulting from your use of
these programs or functions.
****)


(**** Chapter 9.  WRITING INTERPRETERS FOR THE LAMBDA-CALCULUS ****)

use"ParsePrint.ML";           (*Read library, parser, and pretty printer*)

(*** Lambda-terms ***)

signature LAMBDA_NAMELESS =
  sig
  datatype term = Free  of string
		| Bound of int
		| Abs   of string*term
		| Apply of term*term
  val abstract: int -> string -> term -> term
  val subst: int -> term -> term -> term
  val inst: (string * term) list -> term -> term
  end;
  

functor LambdaFUN(Basic: BASIC) : LAMBDA_NAMELESS =
  struct
  local open Basic in
  datatype term = Free  of string
		| Bound of int
		| Abs   of string*term
		| Apply of term*term;

  (*Convert occurrences of b to bound index i in a term*)
  fun abstract i b (Free a) = if a=b then  Bound i  else  Free a
    | abstract i b (Bound j) = Bound j
    | abstract i b (Abs(a,t)) = Abs(a, abstract (i+1) b t)
    | abstract i b (Apply(t,u)) = Apply(abstract i b t, abstract i b u);

  (*Shift by i a term's non-local indexes; d is the depth of abstractions*)
  fun shift 0 d u = u
    | shift i d (Free a) = Free a
    | shift i d (Bound j) = if j>=d then Bound(j+i) else Bound j 
    | shift i d (Abs(a,t)) = Abs(a, shift i (d+1) t)
    | shift i d (Apply(t,u)) = Apply(shift i d t, shift i d u);

  (*Substitute u for bound variable i in a term t*)
  fun subst i u (Free a)  = Free a
    | subst i u (Bound j) =
	if j<i then  	Bound j   	(*locally bound*)
	else if j=i then shift i 0 u
	else (*j>i*)	Bound(j-1)	(*non-local to t*)
    | subst i u (Abs(a,t)) = Abs(a, subst (i+1) u t)
    | subst i u (Apply(t1,t2)) = Apply(subst i u t1, subst i u t2);

  (*Substitution for free variables*)
  fun inst env (Free a) = (inst env (lookup(env,a)) 
		           handle Lookup => Free a)
    | inst env (Bound i) = Bound i
    | inst env (Abs(a,t)) = Abs(a, inst env t)
    | inst env (Apply(t1,t2)) = Apply(inst env t1, inst env t2);
  end
  end;


(*** Parsing of lambda terms ***)
signature PARSELAM = 
  sig
  type term
  val abslist: string list * term -> term
  val applylist: term * term list -> term
  val read: string -> term
  end;

functor ParseLamFUN (structure Parse: PARSE 
		     and       Lambda: LAMBDA_NAMELESS) : PARSELAM =
  struct
  local open Parse  open Lambda  in
  type term = term;

  (*Abstraction over several free variables*)
  fun abslist([],    t) = t
    | abslist(b::bs, t) = Abs(b, abstract 0 b (abslist(bs, t)));

  (*Application of t to several terms*)
  fun applylist(t, []) = t
    | applylist(t, u::us) = applylist(Apply(t,u), us);

  fun makelambda ((((_,b),bs),_),t) = abslist(b::bs,t)

  (*term/atom distinction prevents left recursion; grammar is ambiguous*)
  fun term toks =
   (   $"%" -- id -- repeat id -- $"." -- term	>> makelambda
     || atom -- repeat atom			>> applylist
    ) toks
  and atom toks =
    (   id					>> Free
     || $"(" -- term -- $")"			>> (#2 o #1)
    ) toks;
  val read = reader term;
  end
  end;


(*** Pretty Printing of lambda terms ***)

signature DISPLAM = 
  sig
  type term
  val rename: string list * string -> string
  val stripabs: term -> string list * term
  val pr: term -> unit
  end;

functor DispLamFUN (structure Basic: BASIC 
		    and       Pretty: PRETTY
		    and       Lambda: LAMBDA_NAMELESS) : DISPLAM =
  struct
  local open Basic  open Pretty  open Lambda  in
  type term = Lambda.term;

  (*Free variable in a term -- simple & slow version using append*)
  fun vars (Free a) = [a]
    | vars (Bound i) = []
    | vars (Abs(a,t)) = vars t
    | vars (Apply(t1,t2)) = vars t1 @ vars t2;

  (*rename variable "a" to avoid clashes with the strings bs. *)
  fun rename (bs,a) =
      if  a mem bs  then  rename (bs, a ^ "'")  else  a;

  (*Remove leading lambdas; return bound variable names*)
  fun strip (bs, Abs(a,t)) = 
        let val b = rename (vars t, a)
	in  strip (b::bs, subst 0 (Free b) t)  end
    | strip (bs, u) = (rev bs, u);

  fun stripabs t = strip ([],t);

  fun spacejoin (a,b) = a ^ " " ^ b;

  fun term (Free a) = str a 
    | term (Bound i) = str "??UNMATCHED INDEX??"
    | term (t as Abs _) =
	  let val (b::bs,u) = stripabs t
	      val binder = foldleft spacejoin ("%" ^ b, bs) ^ ". "
	  in  blo(0, [str binder, term u])  end
    | term t = blo(0, applic t)
  and applic (Apply(t,u)) = applic t @ [brk 1, atom u]
    | applic t        = [ atom t ]
  and atom (Free a) = str a 
    | atom t = blo(1, [str"(", term t, str")"]);

  fun pr t = Pretty.pr (std_out, term t, 50);
  end
  end;


(*** Evaluation of lambda terms ***)

signature REDUCE = 
  sig
  type term
  val eval : term -> term
  val byvalue : term -> term
  val headnf : term -> term
  val byname : term -> term
  end;


functor ReduceFUN (Lambda: LAMBDA_NAMELESS) : REDUCE =
  struct
  local open Lambda  in
  type term = term;

  (*evaluation, not affecting function bodies*)
  fun eval (Apply(t1,t2)) = 
        (case eval t1 of
	     Abs(a,u) => eval(subst 0 (eval t2) u)
	   | u1 => Apply(u1, eval t2))
    | eval t = t;

  (*normalization using call-by-value*)
  fun byvalue t = normbodies (eval t)
  and normbodies (Abs(a,t)) = Abs(a, byvalue t)
    | normbodies (Apply(t1,t2)) = Apply(normbodies t1, normbodies t2)
    | normbodies t = t;

  (*head normal form*)
  fun headnf (Abs(a,t)) = Abs(a, headnf t)
    | headnf (Apply(t1,t2)) = 
        (case headnf t1 of
	     Abs(a,t) => headnf(subst 0 t2 t)
	   | u1 => Apply(u1, t2))
    | headnf t = t;

  (*normalization using call-by-name*)
  fun byname t = normargs (headnf t)
  and normargs (Abs(a,t)) = Abs(a, normargs t)
    | normargs (Apply(t1,t2)) = Apply(normargs t1, byname t2)
    | normargs t = t;
  end
  end;


(******** SHORT DEMONSTRATIONS ********)

structure Basic = BasicFUN();
structure LamKey = 
    struct val alphas = []
           and symbols = ["(", ")", "'", "->"]
    end;
structure LamLex = LexicalFUN (structure Basic=Basic and Keyword=LamKey);
structure Parse = ParseFUN(LamLex);
structure Pretty = PrettyFUN();

structure Lambda = LambdaFUN(Basic);
structure ParseLam = ParseLamFUN (structure Parse=Parse and Lambda=Lambda);
structure DispLam = DispLamFUN 
    (structure Basic=Basic and Pretty=Pretty and Lambda=Lambda);
structure Reduce = ReduceFUN(Lambda);

open Basic;  open Lambda; 

val stdenv = map  (fn (a,b) => (a, ParseLam.read b))
[    (*booleans*)
 ("true", "%x y.x"),           ("false",  "%x y.y"), 
 ("if", "%p x y. p x y"),
     (*ordered pairs*)
 ("pair", "%x y f.f x y"),  
 ("fst", "%p.p true"),         ("snd", "%p.p false"),
     (*natural numbers*)
 ("suc", "%n f x. n f (f x)"),
 ("iszero", "%n. n (%x.false) true"),
 ("0", "%f x. x"),             ("1", "suc 0"),
 ("2", "suc 1"),               ("3", "suc 2"),
 ("4", "suc 3"),               ("5", "suc 4"),
 ("6", "suc 5"),               ("7", "suc 6"),
 ("8", "suc 7"),               ("9", "suc 8"),
 ("add",  "%m n f x. m f (n f x)"),
 ("mult", "%m n f. m (n f)"),
 ("expt", "%m n f x. n m f x"),
 ("prefn", "%f p. pair (f (fst p)) (fst p)"),
 ("pre",  "%n f x. snd (n (prefn f) (pair x x))"),
 ("sub",  "%m n. n pre m"),
      (*lists*)
 ("nil",  "%z.z"),
 ("cons", "%x y. pair false (pair x y)"),
 ("null", "fst"),
 ("hd", "%z. fst(snd z)"),     ("tl", "%z. snd(snd z)"),
    (*recursion for call-by-name*)
 ("Y", "%f. (%x.f(x x))(%x.f(x x))"),
 ("fact", "Y (%g n. if (iszero n) 1 (mult n (g (pre n))))"),
 ("append", "Y (%g z w. if (null z) w (cons (hd z) (g (tl z) w)))"),
 ("inflist", "Y (%z. cons MORE z)"),
     (*recursion for call-by-value*)
 ("YV", "%f. (%x.f(%y.x x y)) (%x.f(%y.x x y))"),
 ("factV", "YV (%g n. (if (iszero n) (%y.1) (%y.mult n (g (pre n))))y)") ];


(** first example: parsing/displaying of types **)
structure Type = TypeFUN (structure Parse=Parse and Pretty=Pretty);
Type.read"('a->'b)->('c->'d)";
Type.pr it;

(** lambda reduction examples **)
fun stdread a = inst stdenv (ParseLam.read a);
fun try evfn = DispLam.pr o evfn o stdread;
try Reduce.byvalue "expt 2 3";       (*call-by-value*)
try Reduce.byvalue "sub 9 6";
try Reduce.byvalue "factV 3";
try Reduce.byname "mult 2 3";       (*call-by-name*)
try Reduce.byname "(%x.hello)(Y Y)";   (*diverges under call-by-value*)
try Reduce.byname "hd (tl (Y (%z. append (cons MORE (cons AND nil)) z)))";


