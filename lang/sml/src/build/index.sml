(* copyright CMU ? *)
(* build/index.sml *)

signature INDEX = sig
  val report : Source.inputSource -> (Absyn.dec * Modules.env) -> unit
  val openIndexFile :string -> outstream option
end;

structure Index : INDEX = struct

open Absyn Types Variables Modules

val DEBUG = false

fun index_file_name (name :string) :string =
  let fun split (dirname as ("/"::rest)) filename = (rev dirname, filename)
        | split (c::rest) filename = split rest (c::filename)
	| split [] filename = ([], filename)
      val (dirname, filename) = (split (rev (explode name)) [])
  in implode(dirname @ ("."::"i"::"."::filename))
  end

fun openIndexFile (fname :string) =
    if !System.Control.indexing
    then if !System.Control.markabsyn
         then SOME (open_out(index_file_name fname))
              handle Io s =>
		(System.Print.say("[cannot open index file, "^s^"]\n"); NONE)
         else (System.Print.say
	        "[indexing is turned on, but markabsyn is turned off]\n";
	       NONE)
    else NONE

fun report ({indexStream=NONE,...}:Source.inputSource) _ = ()
  | report (inputSource as {fileName, indexStream = SOME istream,...}) 
           (absyn :Absyn.dec, env: Modules.env) =
  let val current_pos = ref 0 and limit_pos = ref 0
    fun withpos(L1,L2,f) = let val c = !current_pos and l = !limit_pos
	                    in current_pos := L1; limit_pos := L2;
			       f() before (current_pos := c; limit_pos := l)
			       handle e => (current_pos := c; limit_pos := l;
					    raise e)
			   end 

    val and_seq :bool ref = ref(false)

    val formatQid = PrintUtil.formatQid
    val say = outputc istream

    fun nl () = (say "\n"; PPType.resetPPType())

    fun print_type (typ :ty) :unit =
	PrettyPrint.with_pp
	 {consumer=say,linewidth=79,flush=(fn () => flush_out istream)}
	 (fn ppstrm =>
	  (PrettyPrint.add_string ppstrm "(";
	   PPType.ppType env ppstrm typ;
	   PrettyPrint.add_string ppstrm ")"))

    fun print_sym (s: Symbol.symbol) = say (Symbol.name s);

    fun comma_seq elems =
      let fun prElems [el] = say el
	    | prElems (el::rest) = (say el; say ", "; prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

    fun print_and_seq pr elems =
      let val old_and_seq = (!and_seq)
          fun prElems (el::rest) = (pr el; and_seq := true; prElems rest)
	    | prElems [] = ()
       in prElems elems;
	  and_seq := old_and_seq
      end


    fun print_entry (name, f) = 
      let val (_,start_line,_) = Source.filepos inputSource (!current_pos)
	  val (_,end_line,_) = Source.filepos inputSource (!limit_pos) in
        say name; say " "; 
	say ((makestring start_line) ^ " ");
	say ((makestring (!current_pos)) ^ " ");
	say (if (!and_seq) then "A " else "X ");
	say ((makestring end_line) ^ " ");
	say ((makestring (!limit_pos)) ^ " ");
	say "\127 ";
        f();
        nl()
      end;

    fun printPat (VARpat (v as VALvar{typ=ref t,name,...})) =
	  print_entry (formatQid name, fn()=>(say "val "; print_type t))
      | printPat (LAYEREDpat (v,p)) = (printPat(v); printPat(p))
      | printPat (RECORDpat{fields,...}) = app (printPat o #2) fields
      | printPat (VECTORpat (pats,_)) = app printPat pats
      | printPat (APPpat(_,_,p)) = printPat p
      | printPat (CONSTRAINTpat (p,_)) = printPat p
      | printPat _ = ()

    and printDec(VALdec vbs) = 
	       print_and_seq (fn VB{pat,...} => printPat pat) vbs
      | printDec(VALRECdec rvbs) = 
	       print_and_seq (fn RVB{var,...} => printPat(VARpat var)) rvbs
      | printDec(TYPEdec tbs) =
          (print_and_seq
             (fn (TB{tyc=DEFtyc{path=name::_, tyfun=TYFUN{arity,...},...},def}) =>
	       print_entry(Symbol.name name, 
			   fn()=>(say "type ";
				  case arity
				      of 0 => ()
				    | 1 => (say "'a ")
				    | n => (say "(";
					    comma_seq (PPType.typeFormals n);
					    say ") ");
				  print_type def))
	       | _ => ErrorMsg.impossible "Index0")
	      tbs)
      | printDec(DATATYPEdec{datatycs,withtycs}) =
          (print_and_seq 
             (fn GENtyc{path=name::_, arity, kind=ref(DATAtyc dcons),...} =>
                  print_entry(Symbol.name name, fn()=>say "datatype")
                  (* app (fn DATACON{name,...} =>
		          print_entry(Symbol.name name,fn()=>()))
			 dcons *)
               | _ => ErrorMsg.impossible "Index3")
             datatycs)
      | printDec(ABSTYPEdec{abstycs, withtycs, body}) =
          (app (fn GENtyc{path=name::_, kind=ref(DATAtyc dcons),...} =>
                 (print_entry(Symbol.name name, fn()=>say "abstype ");
                  app (fn (DATACON{name,...}) => 
		          print_entry(Symbol.name name,fn()=>()))
		       dcons)
	       | GENtyc{path=name::_,...} =>
	          print_entry(Symbol.name name, fn()=> say "abstype ")
               | _ => ErrorMsg.impossible "Index4")
             abstycs;
	   printDec body)
      | printDec(EXCEPTIONdec ebs) =
          (print_and_seq
             (fn (EBgen{exn=DATACON{name,...},etype,...}) =>
                   print_entry(Symbol.name name,
			       fn()=>(say "exn";
				      case etype of NONE => ()
				     | SOME ty' => (say " of ";
						    print_type ty')))
               | (EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =>
                   print_entry(Symbol.name name,
			       fn()=>(say "exn "; say(Symbol.name dname))))
             ebs)
      | printDec(STRdec sbs) =
          (app (fn (STRB{strvar=STRvar{name,...},def,...}) =>
                 (print_entry(Symbol.name name, fn()=>say "structure");
                  printStrexp def))
             sbs)
      | printDec(ABSdec sbs) = printDec(STRdec sbs)
      | printDec(FCTdec fbs) =
	  let fun printFctExp (FCTfct{def,...}) = printStrexp def
		| printFctExp (VARfct{def=FCTvar{name=fname',...},...}) =
                    print_entry(Symbol.name fname', fn()=> print "functor ")
		| printFctExp (LETfct(dec,fct)) = (
		    printDec dec;
		    printFctExp fct)
	   in (app (fn (FCTB{fctvar=FCTvar{name=fname,...}, def}) =>
                     (print_entry(Symbol.name fname, fn()=> print "functor ");
                      printFctExp def))
               fbs)
	  end
      | printDec(SIGdec sigvars) =
          app (fn SIGvar{name,...} =>
	       print_entry(Symbol.name name, fn()=>say "signature"))
            sigvars
      | printDec(FSIGdec fsigvars) =
          app (fn FSIGvar{name,...} =>
	       print_entry(Symbol.name name, fn()=>say "fsignature"))
            fsigvars
      | printDec(LOCALdec(inner,outer)) = printDec(outer)
      | printDec(SEQdec decs) = app printDec decs
      | printDec(OPENdec strVars) = ()
      | printDec(MARKdec(dec,L1,L2)) = withpos(L1,L2, fn()=>printDec(dec))
      | printDec(FIXdec _) = ()
      | printDec(OVLDdec _) = ()
      | printDec _ = ErrorMsg.impossible "Index2"
    
    and printStrexp(VARstr(STRvar{name,...})) = ()
      | printStrexp(STRUCTstr{body,...}) = app printDec body
      | printStrexp(APPstr{oper=FCTvar{name,...}, argexp,...}) = ()
      | printStrexp(LETstr(dec,body)) = printStrexp(body)
      | printStrexp(MARKstr(body,L1,L2)) =withpos(L1,L2,fn()=>printStrexp body)
  in
    (printDec absyn; nl())
  end

end
