(* Copyright 1989 by AT&T Bell Laboratories *)
(* mcprint.sml *)

signature MCprint =
  sig
    val printCon : Lambda.con -> unit
    val printLexp : Lambda.lexp -> unit
    val printMatch : Modules.env -> (Absyn.pat * Lambda.lexp) list -> unit
    val printFun : Lambda.lexp -> Access.lvar -> unit
  end

structure MCprint : MCprint = struct

open Access Absyn Lambda PrintUtil ErrorMsg

val margin = ref 0
  
val say = System.Print.say

fun indent i = margin := !margin + i
  
exception Undent
  
fun undent i = (margin := !margin - i; if !margin < 0 then raise Undent else ())

fun dent () = tab(!margin)

fun whitespace() =
  let fun ws(n) =
        if n < 0 then raise Undent
	else if n >= 8 then "\t" :: ws(n-8)
	else let val str = case n of 0 => "" | 1 => " " | 2 => "  "
                          | 3 => "   " | 4 => "    " | 5 => "     "
			  | 6 => "      " | _ => "       "
	     in  [str]
	     end
  in  implode(ws(!margin))
  end

fun printCon (DATAcon(sym,rep,_)) = say(Symbol.name sym)
  | printCon (INTcon i) = say (makestring i)
  | printCon (REALcon r) = say r
  | printCon (STRINGcon s) = pr_mlstr s
  | printCon (VLENcon n) = say (makestring n)

(* use of complex in printLexp may lead to stupid n^2 behavior. *)
val rec complex =
     fn VAR w => false
      | FN(_,_,b) => complex b
      | FIX(vl,_,ll,b) => true
      | APP(FN _,_) => true
      | APP(l,r) => complex l orelse complex r
      | SWITCH _ => true
      | RECORD l => 
           let fun f nil = false | f (hd::tl) = complex hd orelse f tl
	    in  f l
	   end
      | VECTOR l => 
           let fun f nil = false | f (hd::tl) = complex hd orelse f tl
	    in  f l
	   end
      | CON(_,l) => complex l
      | DECON(_,l) => complex l
      | SELECT(_,l) => complex l
      | HANDLE _ => true 
      | RAISE(l,_) => complex l
      | INT _ => false | STRING _ => false | REAL _ => false
      | PRIM _ => false
      | WRAP(_,l) => complex l
      | UNWRAP(_,l) => complex l

fun printLexp (VAR v) = say(lvarName v)
  | printLexp (INT i) = say(makestring i)
  | printLexp (REAL s) = say s
  | printLexp (STRING s) = pr_mlstr s
  | printLexp (r as RECORD l) =
	if complex r
	   then (say "RECORD";
		 indent 7;
		 printClosedSequence ("(",",\n"^whitespace(),")") printLexp l;
		 undent 7)
	   else (say "RECORD"; printClosedSequence ("(", ",", ")") printLexp l)
  | printLexp (r as VECTOR l) =
	if complex r
	   then (say "VECTOR";
		 indent 7;
		 printClosedSequence ("(",",\n"^whitespace(),")") printLexp l;
		 undent 7)
	   else (say "VECTOR"; printClosedSequence ("(", ",", ")") printLexp l)
  | printLexp (PRIM(p,_)) = say ("PRIM "^Prim.inLineName p)
  | printLexp (l as SELECT(i,_)) =
	let fun gather(SELECT(i,l)) =
		let val (more,root) = gather l
		in  (i :: more,root)
		end
	      | gather l = (nil,l)
	    val (path,root) = gather l
	    fun ipr (i:int) = say(makestring i)
	in  printLexp root;
	    printClosedSequence ("[",",","]") ipr (rev path)
	end
  | printLexp (FN(v,_,l)) = 
	(say "FN("; say(lvarName v); say ",";
	 if complex l then (newline(); indent 3; dent();
			    printLexp l; say ")"; undent 3)
	 else (printLexp l; say ")")
	 )
  | printLexp (CON((s,c,_),l)) = 
        (say "CON("; say(Symbol.name s); say ",";
	 if complex l then (indent 4; printLexp l; say ")"; undent 4)
	 else (printLexp l; say ")")
	 )
  | printLexp (DECON((s,c,_),l)) = 
        (say "DECON("; say(Symbol.name s); say ",";
	 if complex l then (indent 6; printLexp l; say ")"; undent 6)
	 else (printLexp l; say ")")
	 )
  | printLexp (APP(FN(v,_,l),r)) =
	let val lv = lvarName v
	    val len = size lv + 3
	in  say lv; say " = ";
	    if complex r
		then (indent 2; newline(); dent(); printLexp r; undent 2)
	    else (indent len ; printLexp r; undent len);
	    newline(); dent(); printLexp l
	end
  | printLexp (APP(l,r)) = 
	(say "APP(";
	 if complex l orelse complex r
	   then (indent 4; printLexp l; say ",\n"; dent();
		 printLexp r; say ")"; undent 4)
	   else (printLexp l; say ",";
		 printLexp r; say ")"))
  | printLexp (SWITCH (l,_,llist,default)) =
	let fun switch [(c,l)] =
		  (printCon c; say " => ";
		   indent 8; printLexp l; undent 8)
	      | switch ((c,l)::more) = 
		  (printCon c; say " => ";
		   indent 8; printLexp l; undent 8; newline(); dent();
		   switch more)
	in  say "SWITCH ";
	    indent 7;
	    printLexp l;
	    undent 6;
	    newline(); dent();
	    say "of "; indent 3;
	    switch llist;
	    case (default,llist)
	      of (NONE,_) => ()
	       | (SOME l,nil) =>
		   (say "_ => ";
		    indent 5; printLexp l; undent 5)
	       | (SOME l,_) =>
		   (newline(); dent(); say "_ => ";
		    indent 5; printLexp l; undent 5);
	    undent 4
	end
  | printLexp (FIX(varlist,_,lexplist,lexp)) =
	let fun flist([v],[l]) =
		let val lv = lvarName v
		    val len = size lv + 2
		in  say lv; say ": ";
		    indent len ; printLexp l; undent len
		end
	      | flist(v::vs,l::ls) =
		let val lv = lvarName v
		    val len = size lv + 2
		in  say lv; say ": ";
		    indent len ; printLexp l; undent len;
		    newline(); dent(); flist(vs,ls)
		end
	      | flist(nil,nil) = ()
	in  say "FIX("; indent 4; flist(varlist,lexplist); undent 4;
	    newline(); dent(); say "IN  ";
	    indent 4; printLexp lexp; say ")"; undent 4
	end
  | printLexp (RAISE(l,_)) = (say "RAISE "; indent 6; printLexp l; undent 6)
  | printLexp (HANDLE (lexp,withlexp)) =
      (say "HANDLE "; indent 7; printLexp lexp;
       undent 5; newline(); dent();
       say "WITH "; indent 5; printLexp withlexp; undent 7)
  | printLexp (WRAP(_,l)) = printLexp l
  | printLexp (UNWRAP(_,l)) = printLexp l

val printLexp = (fn l => (printLexp l; newline()))

fun printMatch env ((p,r)::more) =
      (PrettyPrint.with_pp (ErrorMsg.defaultConsumer())
       (fn ppstrm =>
	 (PPAbsyn.ppPat env ppstrm (p,!System.Print.printDepth);
	  PrettyPrint.add_newline ppstrm));
       say " => "; printLexp r; printMatch env more)
  | printMatch _ nil = ()

fun printFun l v =
    let fun last [x] = x | last (a::r) = last r
        val rec findit =
	 fn VAR w => if v=w 
		       then (say("VAR " ^ lvarName v ^ " is free in <lexp>\n");
			     ())
		       else ()
	  | l as FN(w,_,b) => if v=w then printLexp l else findit b
	  | l as FIX(vl,_,ll,b) => if exists (fn w => v=w) vl
				   then printLexp l
				   else (app findit ll; findit b)
	  | APP(l,r) => (findit l; findit r)
	  | SWITCH (l,_,ls,d) =>
		(findit l;
	         app (fn(_,l) => findit l) ls;
		 case d of NONE => () | SOME l => findit l)
	  | RECORD l => app findit l 
	  | VECTOR l => app findit l 
          | SELECT(_,l) => findit l
	  | CON((_,VARIABLE(PATH path),_),e) =>
		(findit(VAR(last path)); findit e)
	  | CON((_,VARIABLEc(PATH path),_),e) =>
		(findit(VAR(last path)); findit e)
	  | CON(_,e) => findit e
	  | DECON((_,VARIABLE(PATH path),_),e) =>
		(findit(VAR(last path)); findit e)
	  | DECON((_,VARIABLEc(PATH path),_),e) =>
		(findit(VAR(last path)); findit e)
	  | DECON(_,e) => findit e
	  | HANDLE(e,h) => (findit e; findit h) 
          | RAISE(l,_) => findit l
	  | INT _ => () | STRING _ => () | REAL _ => ()
	  | PRIM _ => ()
          | WRAP(_,e) => findit e
          | UNWRAP(_,e) => findit e

    in  findit l
    end


end (* struct MCprint *)
