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


(**** Chapter 10.  A TACTICAL THEOREM PROVER ****)

use"ParsePrint.ML";           (*Defines library, parser, and pretty printer*)
use"Imperative.ML";           (*Defines (imperative) sequences*)

(*** Terms and Formulae ***)

signature FOL =
  sig
  datatype term = Var   of string
		| Param of string * string list
		| Bound of int
		| Fun   of string * term list
  datatype form = Pred  of string * term list
		| Conn  of string * form list
		| Quant of string * string * form
  val prec_of: string -> int
  val abstract: int -> term -> form -> form
  val subst: int -> term -> form -> form
  val termvars: term * string list -> string list
  val goalvars: (form list * form list) * string list -> string list
  val termparams: term * (string * string list)list 
                  -> (string * string list)list
  val goalparams: (form list * form list) * (string * string list)list 
                  -> (string * string list)list
  end;
  

functor FolFUN(Basic: BASIC) : FOL =
  struct
  local open Basic in
  datatype term = Var   of string
		| Param of string * string list
		| Bound of int
		| Fun   of string * term list;
  datatype form = Pred  of string * term list
		| Conn  of string * form list
		| Quant of string * string * form;

  (*Replace the term u1 by u2 throughout a term t*)
  fun replace (u1,u2) t =
      if t=u1 then u2 else
      case t of Fun(a,ts) => Fun(a, map (replace(u1,u2)) ts)
	      | _         => t;

  (*Abstraction of a formula over the atomic term t. *)
  fun abstract i t (Pred(a,ts)) = Pred(a, map (replace (t, Bound i)) ts)
    | abstract i t (Conn(b,ps)) = Conn(b, map (abstract i t) ps)
    | abstract i t (Quant(qnt,b,p)) = Quant(qnt, b, abstract (i+1) t p);

  (*Replace (Bound i) by t in formula. t may contain no bound vars *)
  fun subst i t (Pred(a,ts)) = Pred(a, map (replace (Bound i, t)) ts)
    | subst i t (Conn(b,ps)) = Conn(b, map (subst i t) ps)
    | subst i t (Quant(qnt,b,p)) = Quant(qnt, b, subst (i+1) t p);

  (*Precedence table: used by parsing AND display! *)
  fun prec_of "~"   = 4
    | prec_of "&"   = 3
    | prec_of "|"   = 2
    | prec_of "<->" = 1
    | prec_of "-->" = 1
    | prec_of _     = ~1    (*means not an infix*);

  (*Accumulate a term function over all terms in a formula*)
  fun accumform f (Pred(_,ts), xs) = foldright f (ts, xs)
    | accumform f (Conn(_,ps), xs) = foldright (accumform f) (ps, xs)
    | accumform f (Quant(_,_,p), xs) = accumform f (p,xs);

  (*Accumulate a form function over all formulae in a goal [POLYMORHPIC]*)
  fun accumgoal f ((ps,qs), xs) = foldright f (ps, foldright f (qs,xs));

  (*Accumulate all Vars in the term (except in a Param).*)
  fun termvars (Var a, bs) = newmem(a,bs)
    | termvars (Fun(_,ts), bs) = foldright termvars (ts,bs)
    | termvars (_, bs) = bs;
  val goalvars = accumgoal (accumform termvars);

  (*Accumulate all Params*)
  fun termparams (Param(a,bs), pairs) = newmem((a,bs), pairs)
    | termparams (Fun(_,ts), pairs) = foldright termparams (ts,pairs)
    | termparams (_, pairs) = pairs;
  val goalparams = accumgoal (accumform termparams);
  end;
  end;


(*** Parsing of First-order terms & formulae ***)
signature PARSEFOL =
  sig
  type form
  val read: string -> form
  end;


functor ParseFolFUN (structure Parse: PARSE 
		     and       Fol: FOL) : PARSEFOL =
  struct
  local open Parse  open Fol 
    (*One or more phrases separated by commas*)
    fun list ph =    ph -- repeat ($"," -- ph >> #2) >> (op::);

    (*Either (ph,...,ph) or empty. *)
    fun pack ph =    $"(" -- list ph -- $")" >> (#2 o #1)
                  || empty;

    fun term toks =
      (   id   -- pack term	>> Fun
       || $"?" -- id     	>> (Var o #2)  ) toks;

    (*Make a quantifier from parsed information*)
    fun makeQuant (((qnt,b),_),p) = 
	Quant(qnt, b, abstract 0 (Fun(b,[])) p);

    (*Make a connective*)
    fun makeConn a p q = Conn(a, [p,q]);
    fun makeNeg (_,p) = Conn("~", [p]);

    fun form toks =
      (   $"ALL" -- id -- $"." -- form 	>> makeQuant
       || $"EX"  -- id -- $"." -- form 	>> makeQuant
       || infixes (primary,prec_of,makeConn)  ) toks
    and primary toks =
      (   $"~" -- primary       		>> makeNeg
       || $"(" -- form -- $")" 		>> (#2 o #1)
       || id -- pack term 		>> Pred   )  toks;
  in
    type form = Fol.form
    val read = reader form
  end
  end;


(*** Pretty Printing of terms, formulae, and goals ***)
signature DISPFOL =
  sig
  type form
  val pr_form: form -> unit
  val pr_goal: int -> form list * form list -> unit
  end;


functor DispFolFUN (structure Basic: BASIC 
		    and       Pretty: PRETTY
		    and       Fol: FOL) : DISPFOL =
  struct
  local open Basic  open Pretty  open Fol  
    fun enclose sexp = blo(1, [str"(", sexp, str")"]); 

    fun commas [] = []
      | commas (sexp::sexps) = str"," :: brk 1 :: sexp :: commas sexps;

    fun list (sexp::sexps) = blo(0, sexp :: commas sexps);  

    fun term (Param(a,_)) = str a 
      | term (Var a) = str ("?"^a)
      | term (Bound i) = str "??UNMATCHED INDEX??"
      | term (Fun (a,ts)) =  blo(0, [str a, args ts])
    and args [] = str""
      | args ts = enclose (list (map term ts));

    (*display formula in context of operator with precedence k *)
    fun form k (Pred (a,ts)) = blo(0, [str a, args ts])
      | form k (Conn("~", [p])) = blo(0, [str "~", form (prec_of "~") p])
      | form k (Conn(C, [p,q])) =
	    let val pf = form (maxl[prec_of C, k])
		val sexp = blo(0, [pf p, str(" "^C), brk 1, pf q])
	    in  if (prec_of C <= k) then (enclose sexp) else sexp
	    end
      | form k (Quant(qnt,b,p)) =
	    let val q = subst 0 (Fun(b,[])) p
		val sexp = blo(2, 
		     [str(qnt ^ " " ^ b ^ "."), brk 1,  form 0 q])
	    in  if  k>0  then  (enclose sexp)  else sexp  end
      | form k _ = str"??UNKNOWN FORMULA??";

    fun formlist [] = str"empty"
      | formlist ps = list (map (form 0) ps);
  in
    type form = Fol.form;

    fun pr_form p = pr (std_out, form 0 p, 50);

    fun pr_goal (n:int) (ps,qs) = 
       pr (std_out, 
	   blo (4, [str(" "  ^ makestring n  ^  ". "), 
	            formlist ps, brk 2, str"|-  ", formlist qs]),
           50);
  end;
  end;


(*** UNIFICATION ***)
signature UNIFY =
  sig
  type term and form
  exception Fail
  val atoms: form * form -> (string*term)list
  val instterm: (string*term)list -> term -> term
  val instform: (string*term)list -> form -> form
  val instgoal: (string*term)list -> (form list * form list) -> 
		(form list * form list)
  end;


functor UnifyFUN (structure Basic: BASIC and Fol: FOL) : UNIFY =
  struct
  local open Basic  open Fol   in
  type term = Fol.term and form = Fol.form
  exception Fail;

  (*Naive unification of terms containing no bound variables*)
  fun unifylists env =
    let fun chase (Var a) =  (*Chase variable assignments*)
	      (chase(lookup(env,a)) handle Lookup => Var a)
	  | chase t = t
	fun occurs a (Fun(_,ts)) = occsl a ts
	  | occurs a (Param(_,bs)) = occsl a (map Var bs)
	  | occurs a (Var b) =  (a=b) orelse 
	      (occurs a (lookup(env,b))  handle Lookup => false)
	  | occurs a _ = false
	and occsl a = exists (occurs a)
	and unify (Var a, t) = 
	      if t = Var a  then  env  else
	      if occurs a t then  raise Fail  else  (a,t)::env
	  | unify (t, Var a) = unify (Var a, t)
	  | unify (Param(a,_), Param(b,_)) =
	      if a=b then env  else  raise Fail
	  | unify (Fun(a,ts), Fun(b,us)) =  
	      if a=b then unifyl(ts,us) else raise Fail
	  | unify _ =  raise Fail
       and unifyl ([],[]) = env
	 | unifyl (t::ts, u::us) =
               unifylists (unify (chase t, chase u)) (ts,us)
	 | unifyl _ =  raise Fail
      in  unifyl  end

  (*Unification of atomic formulae*)
  fun atoms (Pred(a,ts), Pred(b,us)) =  
	  if a=b then unifylists [] (ts,us)  else  raise Fail
    | atoms _ =  raise Fail;

  (*Instantiate a term by an environment*)
  fun instterm env (Fun(a,ts)) = Fun(a, map (instterm env) ts)
    | instterm env (Param(a,bs)) =
	Param(a, foldright termvars (map (instterm env o Var) bs, []))
    | instterm env (Var a) = (instterm env (lookup(env,a))
			      handle Lookup => Var a)
    | instterm env t = t;

  (*Instantiate a formula*)
  fun instform env (Pred(a,ts))   = Pred(a, map (instterm env) ts)
    | instform env (Conn(b,ps))   = Conn(b, map (instform env) ps)
    | instform env (Quant(qnt,b,p)) = Quant(qnt, b, instform env p);

  fun instgoal env (ps,qs) = 
        (map (instform env) ps, map (instform env) qs);
  end
  end;


(*** Tactics and proof states -- the abstract type "state" ***)
signature PROOF =
  sig
  structure Seq: SEQUENCE
  type state and form
  val maingoal: state -> form
  and subgoals: state -> (form list * form list)list
  val initial: form -> state
  and final: state -> bool
  val unify_tac: int -> state -> state Seq.T
  and conj_left_tac: int -> state -> state Seq.T
  and conj_right_tac: int -> state -> state Seq.T
  and disj_left_tac: int -> state -> state Seq.T
  and disj_right_tac: int -> state -> state Seq.T
  and imp_left_tac: int -> state -> state Seq.T
  and imp_right_tac: int -> state -> state Seq.T
  and neg_left_tac: int -> state -> state Seq.T
  and neg_right_tac: int -> state -> state Seq.T
  and iff_left_tac: int -> state -> state Seq.T
  and iff_right_tac: int -> state -> state Seq.T
  and all_left_tac: int -> state -> state Seq.T
  and all_right_tac: int -> state -> state Seq.T
  and ex_left_tac: int -> state -> state Seq.T
  and ex_right_tac: int -> state -> state Seq.T
  end;


functor ProofFUN (structure Basic: BASIC and Fol: FOL and Unify: UNIFY
                  and Seq: SEQUENCE
     sharing type Fol.form=Unify.form) : PROOF =
  struct
  local open Basic  open Fol   in
  structure Seq = Seq;

  type form = Fol.form and goal = form list * form list;

  (*A state contains subgoals, main goal, variable counter *)
  datatype state = State of goal list * form * int;

  fun maingoal (State(gs,p,_)) = p
  and subgoals (State(gs,p,_)) = gs;

  (*initial state has one subgoal *)
  fun initial p = State([ ([],[p]) ], p, 0);

  (*final state has no subgoals.*)
  fun final (State([],p,_)) = true
    | final (State(_,p,_)) = false;

  (*add the goals "newgs" to replace subgoal i*)
  fun splicegoals gs newgs i = take(i-1,gs) @ newgs @ drop(i,gs);

  (*Solves goal p|-q by unifying p with q, if both are atomic formulae.
    Returns list of successful environments. *)
  fun unifiable ([], _) = Seq.empty
    | unifiable (p::ps, qs) = 
	let fun find [] = unifiable (ps,qs)
	      | find (q::qs) = Seq.cons(Unify.atoms(p,q), fn() => find qs)
			       handle Unify.Fail => find qs
	in  find qs  end;

  fun atomic (Pred _) = true
    | atomic _ = false;

  fun inst [] st = st           (*no copying if environment is empty*)
    | inst env (State(gs,p,n)) =
        State (map (Unify.instgoal env) gs,  Unify.instform env p, n);

  (*for "basic" goals with unifiable atomic formulae on opposite sides*)
  fun unify_tac i (State(gs,p,n)) =
    let val (ps,qs) = nth(gs,i-1)
	fun next env = inst env (State(splicegoals gs [] i, p, n))
    in Seq.map next (unifiable(filter atomic ps, filter atomic qs)) end
    handle Nth => Seq.empty;


  (*** Tactics for propositional rules ***)
  exception TacticFailed;    (* the tactic cannot be applied *)

  (* return & delete first formula of the form Conn(a,_,_) *)
  fun splitconn a qs =
    let fun get [] = raise TacticFailed
	  | get (Conn(b,ps) :: qs) = if a=b then ps else get qs
	  | get (q::qs) = get qs;
	fun del [] = []
	  | del ((q as Conn(b,_)) :: qs) = 
	        if a=b then qs else q :: del qs
	  | del (q::qs) = q :: del qs
    in (get qs, del qs)  end;

  (*goalfun maps goal -> goal list; this operator makes a tactic*)
  fun SUBGOAL goalf i (State(gs,p,n)) =
      let val gs2 = splicegoals gs (goalf (nth(gs,i-1))) i
      in  Seq.cons (State(gs2, p, n),  fn()=>Seq.empty)  end
      handle _ => Seq.empty;

  val conj_left_tac = SUBGOAL (fn (ps,qs) =>
      let val ([p1,p2], ps') = splitconn "&" ps
      in  [ (p1::p2::ps', qs) ]  end);

  val conj_right_tac = SUBGOAL (fn (ps,qs) =>
      let val ([q1,q2], qs') = splitconn "&" qs
      in  [ (ps, q1::qs'),    (ps, q2::qs') ]  end);

  val disj_left_tac = SUBGOAL (fn (ps,qs) =>
      let val ([p1,p2], ps') = splitconn "|" ps
      in  [ (p1::ps', qs),    (p2::ps', qs)]   end);

  val disj_right_tac = SUBGOAL (fn (ps,qs) =>
      let val ([q1,q2], qs') = splitconn "|" qs
      in  [ (ps, q1::q2::qs') ]  end);

  val imp_left_tac = SUBGOAL (fn (ps,qs) =>
      let val ([p1,p2], ps') = splitconn "-->" ps
      in  [ (p2::ps', qs),    (ps', p1::qs)]  end);

  val imp_right_tac = SUBGOAL (fn (ps,qs) =>
      let val ([q1,q2], qs') = splitconn "-->" qs
      in  [ (q1::ps, q2::qs') ]  end);

  val neg_left_tac = SUBGOAL (fn (ps,qs) =>
      let val ([p], ps') = splitconn "~" ps
      in  [ (ps', p::qs)]  end);

  val neg_right_tac = SUBGOAL (fn (ps,qs) =>
      let val ([q], qs') = splitconn "~" qs
      in  [ (q::ps, qs') ]  end);

  val iff_left_tac = SUBGOAL (fn (ps,qs) =>
      let val ([p1,p2], ps') = splitconn "<->" ps
      in  [ (p1::p2::ps', qs),    (ps', p1::p2::qs)]  end);

  val iff_right_tac = SUBGOAL (fn (ps,qs) =>
      let val ([q1,q2], qs') = splitconn "<->" qs
      in  [ (q1::ps, q2::qs'),    (q2::ps, q1::qs') ]  end);


  (*** Tactics for quantifier rules ***)

  (* return & delete first formula of the form Quant(qnt,_,_) *)
  fun splitquant qnt qs =
    let fun get [] = raise TacticFailed
	  | get ((q as Quant(qnt2,_,p)) :: qs) = 
	      if  qnt=qnt2  then  q  else  get qs
	  | get (q::qs) = get qs;
	fun del [] = []
	  | del ((q as Quant(qnt2,_,p)) :: qs) = 
	      if  qnt=qnt2  then  qs  else q :: del qs
	  | del (q::qs) = q :: del qs
    in (get qs, del qs)  end;

  fun letter n = chr(ord("a")+n);

  fun gensym n = (* the "_" prevents clashes with user's variable names*)
     if n<26 then "_" ^ letter n
     else gensym(n div 26) ^ letter(n mod 26);

  fun SUBGOAL_SYM goalf i (State(gs,p,n)) =
      let val gs2 = splicegoals gs (goalf (nth(gs,i-1), gensym n)) i
      in  Seq.cons (State(gs2, p, n+1),  fn()=>Seq.empty)  end
      handle _ => Seq.empty;

  val all_left_tac = SUBGOAL_SYM (fn ((ps,qs), b) =>
      let val (qntform as Quant(_,_,p), ps') = splitquant "ALL" ps
	  val px = subst 0 (Var b) p
      in  [ (px :: ps' @ [qntform], qs) ]  end);

  val all_right_tac = SUBGOAL_SYM (fn ((ps,qs), b) =>
      let val (Quant(_,_,q), qs') = splitquant "ALL" qs
	  val vars = goalvars ((ps,qs), [])
	  val qx = subst 0 (Param(b, vars)) q
      in  [ (ps, qx::qs') ]  end);

  val ex_left_tac = SUBGOAL_SYM (fn ((ps,qs), b) =>
      let val (Quant(_,_,p), ps') = splitquant "EX" ps
	  val vars = goalvars ((ps,qs), [])
	  val px = subst 0 (Param(b, vars)) p
      in  [ (px::ps', qs) ]  end);

  val ex_right_tac = SUBGOAL_SYM (fn ((ps,qs), b) =>
      let val (qntform as Quant(_,_,q), qs') = splitquant "EX" qs
	  val qx = subst 0 (Var b) q
      in  [ (ps, qx :: qs' @ [qntform]) ]  end);
  end
  end;


(*** Commands to modify the top-level proof state ***)

signature COMMAND =
  sig
  structure Seq: SEQUENCE
  type state
  val goal: string -> unit
  val by: (state -> state Seq.T) -> unit
  val pr: state -> unit
  val state: unit -> state
  end;


functor CommandFUN (structure Basic: BASIC and Fol: FOL
          and ParseFol: PARSEFOL and DispFol: DISPFOL and Proof: PROOF
    sharing type Fol.form=ParseFol.form=DispFol.form=Proof.form): COMMAND =
  struct
  local open Basic  in
  structure Seq = Proof.Seq;
  type state = Proof.state;

  val curr_state = ref (Proof.initial (Fol.Pred("No goal yet!",[])));

  fun question s = " ?" ^ s;
  fun printpar (a,[]) = ()     (*print a line of parameter table*)
    | printpar (a,ts) = 
        output(std_out, a ^ " not in " ^ implode (map question ts) ^ "\n");

  fun printgoals (_, []) = ()
    | printgoals (n, g::gs) = (DispFol.pr_goal n g;  printgoals (n+1,gs));

  fun pr st =  (*print a proof state*)
      let val p = Proof.maingoal st  and  gs = Proof.subgoals st
      in  DispFol.pr_form p;
	  if Proof.final st then  output(std_out, "No subgoals left!\n")
	  else (printgoals (1,gs);
	        map printpar (foldright Fol.goalparams (gs, []));  ())
      end;

  (*print new state, then set it*)
  fun setstate state = (pr state;  curr_state := state);

  fun goal aform = setstate (Proof.initial (ParseFol.read aform));

  fun by tac = setstate (Seq.hd (tac (!curr_state)))
          handle Seq.E => output(std_out, "** Tactic FAILED! **\n")

  fun state() = !curr_state;
  end
  end;


(*** Tacticals ***)

infix 5 THEN;
infix 0 ORELSE;
infix 0 ORI;
infix 0 APPEND;

signature TACTICAL =
  sig
  structure Seq: SEQUENCE
  val THEN: ('a -> 'b Seq.T) * ('b -> '_c Seq.T) -> 'a -> '_c Seq.T
  val ORELSE: ('a -> 'b Seq.T) * ('a -> 'b Seq.T) -> 'a -> 'b Seq.T
  val APPEND: ('a -> '_b Seq.T) * ('a -> '_b Seq.T) -> 'a -> '_b Seq.T
  val all_tac: '_a -> '_a Seq.T
  val no_tac: 'a -> 'b Seq.T
  val TRY: ('_a -> '_a Seq.T) -> '_a -> '_a Seq.T
  val REPEAT: ('_a -> '_a Seq.T) -> '_a -> '_a Seq.T
  val DEPTH_FIRST: ('_a -> bool) -> ('_a -> '_a Seq.T) -> '_a -> '_a Seq.T
  val ORI: (int -> 'a -> 'b Seq.T) * (int -> 'a -> 'b Seq.T) -> 
	    int -> 'a -> 'b Seq.T
  end;


functor TacticalFUN (Proof: PROOF) : TACTICAL  =
  struct
  structure Seq = Proof.Seq;

  (*flatten a sequence of sequences, taking care not to loop! *)
  local open Seq in
    fun flatseq xqq =
      if null xqq then empty
      else if null(hd xqq) then flatseq(tl xqq)
      else cons(hd(hd xqq),  fn()=> append(tl(hd xqq), flatseq(tl xqq)));
  end;

  (*THEN performs one tactic followed by another*)
  fun (tac1 THEN tac2) st = flatseq (Seq.map tac2 (tac1 st));

  (*ORELSE commits to the first successful tactic: no backtracking. *)
  fun (tac1 ORELSE tac2) st = 
      let val st1 = tac1 st 
      in  if Seq.null st1  then  tac2 st  else st1  end;

  (*APPEND combines the results of two tactics with backtracking.*)
  fun (tac1 APPEND tac2) st = 
      flatseq(Seq.cons(tac1 st,  (*delay application of tac2!*)
		       fn()=> Seq.cons(tac2 st, fn()=> Seq.empty)));

  (*accepts all states unchanged;  identity of THEN*)
  fun all_tac st = Seq.cons(st, fn()=> Seq.empty);

  (*accepts no states;  identity of ORELSE and APPEND*)
  fun no_tac st = Seq.empty;

  fun TRY tac = tac ORELSE all_tac;

  (*Performs no backtracking: quits when it gets stuck*)
  fun REPEAT tac st = (tac THEN REPEAT tac ORELSE all_tac) st;

  (*Repeats again and again until "pred" reports proof tree as satisfied*)
  fun DEPTH_FIRST pred tac st =
     (if pred st  then  all_tac 
		  else  tac THEN DEPTH_FIRST pred tac) st;

  (*For combining int->tactic functions with ORELSE*)
  fun (f1 ORI f2) i = f1 i ORELSE f2 i;
  end;


(*** An automatic tactic for first-order logic ***)

signature FOLTAC =
  sig
  structure Seq: SEQUENCE
  type state
  val safe_step_tac: int -> state -> state Seq.T
  val quant_tac: int -> state -> state Seq.T
  val step_tac: int -> state -> state Seq.T
  val depth_tac: state -> state Seq.T
  end;


functor FolTacFUN (structure Proof: PROOF and Tactical: TACTICAL
     sharing Proof.Seq = Tactical.Seq) : FOLTAC  =
  struct
  local  open Proof  Tactical  in
  structure Seq = Seq;
  type state = state;

  (*Deterministic; applies one rule to the goal; no unification or variable
    instantiation; cannot render the goal unprovable.*)
  val safe_step_tac =
      (*1 subgoal*)
      conj_left_tac ORI disj_right_tac ORI imp_right_tac ORI 
      neg_left_tac ORI neg_right_tac ORI   
      ex_left_tac ORI all_right_tac ORI 
      (*2 subgoals*)
      conj_right_tac ORI disj_left_tac ORI imp_left_tac ORI 
      iff_left_tac ORI iff_right_tac;

  (*expand a quantifier on the left and the right (if possible!) *)
  fun quant_tac i = TRY (all_left_tac i) THEN TRY (ex_right_tac i);

  val step_tac = unify_tac ORI safe_step_tac ORI quant_tac;

  val depth_tac = DEPTH_FIRST final (step_tac 1);
  end
  end;


(******** SHORT DEMONSTRATIONS ********)

structure Basic = BasicFUN();
structure FolKey = 
    struct val alphas = ["ALL","EX"]
           and symbols = ["(", ")", ".", ",", "?", "~", "&", "|", 
                          "<->", "-->", "|-"]
    end;
structure FolLex = LexicalFUN (structure Basic=Basic and Keyword=FolKey);
structure Parse = ParseFUN(FolLex);
structure Pretty = PrettyFUN();
structure Fol = FolFUN(Basic);
structure ParseFol = ParseFolFUN(structure Parse=Parse and Fol=Fol);
structure DispFol = DispFolFUN 
    (structure Basic=Basic and Pretty=Pretty and Fol=Fol);
structure Unify = UnifyFUN(structure Basic=Basic and Fol=Fol);
structure Proof = ProofFUN(structure Basic=Basic 
                         and Unify=Unify and Seq=Seq and Fol=Fol);
structure Command = CommandFUN(structure Basic=Basic 
            and Fol=Fol and ParseFol=ParseFol
            and DispFol=DispFol and Proof=Proof);
structure Tactical = TacticalFUN(Proof);
structure FolTac = FolTacFUN (structure Proof=Proof and Tactical=Tactical);
open Fol Proof Command Tactical FolTac;

(* the second quantifier proof, section 10.4 *)
goal "EX z. P(z) --> (ALL x. P(x))";
by (ex_right_tac 1);
by (imp_right_tac 1);
by (all_right_tac 1);
by (unify_tac 1);  (*FAILS!*)
by (ex_right_tac 1);
by (imp_right_tac 1);
by (unify_tac 1);


(** Problems by Pelletier (1986). **)

(*3*)
goal "~(P-->Q) --> (Q-->P)";
by depth_tac;

(*5*)
goal "((P|Q)-->(P|R)) --> (P|(Q-->R))";
by depth_tac;

(*8.  Peirce's law*)
goal "((P-->Q) --> P)  -->  P";
by depth_tac;

(*12.  "Dijkstra's law"*)
goal "((P <-> Q) <-> R)  -->  (P <-> (Q <-> R))";
by depth_tac;

(*13.  Distributive law*)
goal "P | (Q & R)  <-> (P | Q) & (P | R)";
by depth_tac;

(*17*)
goal "((P & (Q-->R))-->S)  <->  ((~P | Q | S) & (~P | ~R | S))";
by depth_tac;

(*19*)
goal "EX x. ALL y. ALL z. (P(y)-->Q(z)) --> (P(x)-->Q(x))";
by depth_tac;

(*20*)
goal "(ALL x. ALL y. EX z. ALL w. (P(x)&Q(y)-->R(z)&S(w)))	\
\   --> (EX x. EX y. P(x) & Q(y)) --> (EX z. R(z))";
by depth_tac; 

(*21*)
goal "(EX x. P-->Q(x)) & (EX x. Q(x)-->P) --> (EX x. P<->Q(x))";
by depth_tac; 

(*22*)
goal "~(EX x. S(x)&Q(x)) & (ALL x. P(x) --> Q(x)|R(x)) &	\
\    ~(EX x.P(x)) --> (EX x.Q(x)) & (ALL x. Q(x)|R(x) --> S(x))	\
\   --> (EX x. P(x)&R(x))";
by depth_tac; 

(*27*)
goal "(EX x. P(x) & ~Q(x)) &	\
\       (ALL x. P(x) --> R(x)) &	\
\       (ALL x. M(x) & L(x) --> P(x)) &	\
\       ((EX x. R(x) & ~ Q(x)) --> (ALL x. L(x) --> ~ R(x)))	\
\   --> (ALL x. M(x) --> ~L(x))";
by depth_tac; 

(*32*)
goal "(ALL x. P(x) & (Q(x)|R(x))-->S(x)) &	\
\       (ALL x. S(x) & R(x) --> L(x)) &		\
\       (ALL x. M(x) --> R(x))	\
\   --> (ALL x. P(x) & M(x) --> L(x))";
by depth_tac;

(*37*)
goal "(ALL z. EX w. ALL x. EX y.					\
\          (P(x,z)-->P(y,w)) & P(y,z) & (P(y,w) --> (EX u.Q(u,w)))) &	\
\       (ALL x. ALL z. ~P(x,z) --> (EX y. Q(y,z))) &			\
\       ((EX x. EX y. Q(x,y)) --> (ALL x. R(x,x)))			\
\   --> (ALL x. EX y. R(x,y))";
by depth_tac;

(*40*)
goal "(EX y. ALL x. J(y,x) <-> ~J(x,x))  	\
\       -->  ~ (ALL x. EX y. ALL z. J(z,y) <-> ~ J(z,x))";
by depth_tac;
