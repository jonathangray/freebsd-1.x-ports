(* 1989, 1990, 1991 by AT&T Bell Laboratories *)
(* ppbasics.sml *)

signature PPBASICS = 
sig
    val ppTuple: PrettyPrint.ppstream
                 -> (PrettyPrint.ppstream -> 'a -> unit) -> 'a list -> unit
    val ppFormals : PrettyPrint.ppstream -> int -> unit
    val ppAccess: PrettyPrint.ppstream -> Access.access -> unit
    val ppRep: PrettyPrint.ppstream -> Access.conrep -> unit
    val ppDcon: PrettyPrint.ppstream -> Types.datacon -> unit
    val ppVar: PrettyPrint.ppstream -> Variables.var -> unit
    val ppSignature: PrettyPrint.ppstream 
                     -> Modules.env * Modules.Signature * int -> unit
    val ppSignatureVar: PrettyPrint.ppstream
                        -> Modules.env * Modules.signatureVar * int -> unit 
    val ppFunsigVar: PrettyPrint.ppstream
                     -> Modules.env * Modules.funsigVar * int -> unit 
    val ppStructure: PrettyPrint.ppstream
                     -> Modules.env * Modules.Structure * int -> unit
    val ppStructureVar: PrettyPrint.ppstream
                        -> Modules.env * Modules.structureVar * int -> unit
    val ppStructureName : PrettyPrint.ppstream
                          -> Modules.env * Modules.Structure -> unit
    val ppBinding: PrettyPrint.ppstream
                   -> Modules.env * Modules.binding * int  -> unit
    val ppFunctor : PrettyPrint.ppstream
                    -> Modules.env * Modules.Functor * int -> unit
    val ppFunctorVar : PrettyPrint.ppstream
                       -> Modules.env * Modules.functorVar * int -> unit
    val ppEnv : PrettyPrint.ppstream
                -> Modules.env * Modules.env * int * Symbol.symbol list option
                -> unit
    val ppDebugDcon : PrettyPrint.ppstream
                      -> Modules.env -> Types.datacon -> unit
    val ppDebugVar: PrettyPrint.ppstream 
                    -> Modules.env -> Variables.var -> unit
end

structure PPBasics : PPBASICS = struct

structure PP = PrettyPrint;
open PrettyPrint PPUtil Access Variables Types Fixity Modules

val internals = System.Control.internals

fun C f x y = f y x;

val pps = add_string
fun ppi ppstrm (i:int) = pps ppstrm (makestring i)
val ppType = PPType.ppType
val ppTycon = PPType.ppTycon
fun add_comma ppstrm = pps ppstrm ","
fun add_comma_nl ppstrm  = (add_comma ppstrm; add_newline ppstrm)
fun nl_indent ppstrm i = let val {linewidth,...} = dest_ppstream ppstrm 
                         in
                         add_break ppstrm (linewidth,i)
                         end;
fun nl_app ppstrm f =
   let fun g [] = ()
         | g [el] = f ppstrm el
         | g (el::rst) = (f ppstrm el; add_newline ppstrm; g rst)
   in  g
   end;

fun br_app ppstrm f =
   let fun g [] = ()
         | g [el] = f ppstrm el
         | g (el::rst) = (f ppstrm el; add_break ppstrm (1,0); g rst)
   in  g
   end;

fun en_pp ppstrm = {begin_block = PrettyPrint.begin_block ppstrm, 
                    end_block = fn () => PrettyPrint.end_block ppstrm,
                    pps = PrettyPrint.add_string ppstrm,
                    add_break = PrettyPrint.add_break ppstrm,
                    add_newline = fn () => PrettyPrint.add_newline ppstrm};

fun ppTyfun ppstrm env (TYFUN{arity,body}) =
   let val {begin_block, end_block, pps, add_break,...} = en_pp ppstrm
   in
   (begin_block INCONSISTENT 2;
    pps "TYFUN({arity="; 
    ppi ppstrm arity; add_comma ppstrm;
    add_break(0,0);
    pps "body="; 
    ppType env ppstrm body; 
    pps "})";
    end_block())
   end;

fun ppArray ppstrm (f:ppstream -> 'a -> unit, a:'a array) =
    let val {begin_block,pps,add_break,end_block,...} = en_pp ppstrm
        fun loop i = 
	    let val elem = Array.sub(a,i)
	     in pps (makestring i);
		pps ": "; 
		f ppstrm elem;
		add_break (1,0);
		loop (i+1)
	    end
     in begin_block INCONSISTENT 0;
	loop 0 handle Array.Subscript => ();
        end_block()
    end

fun ppTuple ppstrm f =
    ppClosedSequence ppstrm 
      {front=C pps "(",
       sep=fn ppstrm => (pps ppstrm ",";add_break ppstrm (0,0)),
       back=C pps ")",
       pr=f, style=INCONSISTENT}

fun ppFormals ppstrm =
   let fun ppF 0 = ()
         | ppF 1 = pps ppstrm " 'a"
         | ppF n = (pps ppstrm " ";
                    ppTuple ppstrm (fn ppstrm => fn s => pps ppstrm ("'"^s))
                                   (PPType.typeFormals n))
   in  ppF
   end;

fun ppAccess ppstrm a = pps ppstrm (" ["^(Access.pr_access a)^"]");

fun ppRep ppstrm =
   let val {pps,...} = en_pp ppstrm
       val ppi = pps o (makestring:int->string)
       val ppAccess = ppAccess ppstrm
       fun ppR UNTAGGED = pps "UNTAGGED"
         | ppR (TAGGED i) = (pps "TAGGED["; ppi i; pps "]")
         | ppR (CONSTANT i) = (pps "CONSTANT["; ppi i; pps "]")
         | ppR (TAGGEDREC(i,j)) =
               (pps "TAGGEDREC["; ppi i; add_comma ppstrm; ppi j; pps "]")
         | ppR (UNTAGGEDREC i) = (pps "UNTAGGEDREC["; ppi i; pps "]")
         | ppR TRANSPARENT = pps "TRANSPARENT"
         | ppR REF = pps "REF"
         | ppR (VARIABLE a) = (pps "VARIABLE["; ppAccess a; pps "]")
         | ppR (VARIABLEc a) = (pps "VARIABLEc["; ppAccess a; pps "]")
   in  ppR
   end;

fun ppDcon ppstrm =
   let fun ppD(DATACON{name,rep=VARIABLE(access),...}) =
              (ppSym ppstrm (name); 
               if !internals then ppAccess ppstrm access else ())
         | ppD(DATACON{name,rep=VARIABLEc(access),...}) =
              (ppSym ppstrm (name); 
               if !internals then ppAccess ppstrm access else ())
         | ppD(DATACON{name,...}) = ppSym ppstrm (name)
   in  ppD
   end;

fun ppDebugDcon ppstrm env (DATACON{name,rep,const,typ,sign}) =
   let val {begin_block,end_block,pps,add_break,...} = en_pp ppstrm
       val ppSym = ppSym ppstrm
   in
   (begin_block CONSISTENT 3;
    pps "DATACON";
    add_break(0,0);
    pps "{name = "; ppSym name; add_comma_nl ppstrm;
    pps "const = "; pps (makestring const); add_comma_nl ppstrm;
    pps "typ = "; ppType env ppstrm typ; add_comma_nl ppstrm;
    pps "rep ="; ppRep ppstrm rep; add_comma_nl ppstrm;
(*     pps "sign = ["; ppvseq ppstrm 0 "," (fn x=>ppRep x) sign; pps "]}"; *)
     pps "sign = ["; ppvseq ppstrm 0 "," ppRep sign; pps "]}";
    end_block())
   end;

(* dconTyc: get the range type of a data constructor *)

fun dconTyc(DATACON{typ,const,...}) =
    let fun f (POLYty{tyfun=TYFUN{body,...},...},b) = f (body,b)
	  | f (CONty(tyc,_),true) = tyc
	  | f (CONty(_,[_,CONty(tyc,_)]),false) = tyc
	  | f _ = ErrorMsg.impossible "ppdec.dconTyc"
    in f (typ,const)
    end

fun ppDatacon (env:Modules.env,DATACON{name,typ,...}) ppstrm =
   let val {begin_block,end_block,pps,...} = en_pp ppstrm
   in  (begin_block INCONSISTENT 0;
        ppSym ppstrm name; pps " : "; ppType env ppstrm typ;
        end_block())
   end

fun ppConBinding ppstrm =
 let val {begin_block,end_block,pps,...} = en_pp ppstrm
     fun ppCon(DATACON{name,typ,rep=VARIABLE _,...},env) =
             (begin_block CONSISTENT 0;
              pps "exception "; ppSym ppstrm name; pps " of "; 
              ppType env ppstrm (BasicTypes.domain typ);
              end_block())
       | ppCon (DATACON{name,typ,rep=VARIABLEc _,...},env) =
              (begin_block CONSISTENT 0;
               pps "exception "; ppSym ppstrm name;
               end_block())
       | ppCon (con,env) = 
           let exception Hidden
               val visibleDconTyc =
                   case dconTyc con
                     of RELtyc _ => true (* datacons always visible in sigs *)
                      | tyc =>
                            (TypesUtil.equalTycon
                             (ModuleUtil.lookTYC (env,[TypesUtil.tycName tyc],
                                                  fn _ => raise Hidden),
                              tyc)
                            handle Hidden => false)
           in if !internals orelse not visibleDconTyc 
           then (begin_block CONSISTENT 0;
                 pps "con ";
                 ppDatacon(env,con) ppstrm;
                 end_block())
	   else ()
           end
 in  ppCon
 end;

(* Support for a hack to make sure that non-visible ConBindings don't
   cause spurious blank lines when pp-ing signatures.
*)
fun is_ppable_ConBinding (DATACON{name,typ,rep=VARIABLE _,...},_) = true
  | is_ppable_ConBinding (DATACON{name,typ,rep=VARIABLEc _,...},_) = true
  | is_ppable_ConBinding (con,env) = 
      let exception Hidden
	  val visibleDconTyc =
	        case dconTyc con
		  of RELtyc _ => true (* datacons are always visible in sigs *)
		   | tyc => (TypesUtil.equalTycon
			      (ModuleUtil.lookTYC (env,[TypesUtil.tycName tyc],
					      fn _ => raise Hidden),
			       tyc)
			     handle Hidden => false)
       in (!internals orelse not visibleDconTyc)
      end

fun all_ppable_bindings alist env = 
    fold (fn (c as CONbind con,L) => 
                 if (is_ppable_ConBinding(con,env))
                 then c::L
                 else L
	   | (s as STRbind(STRvar{name,...}),L) =>
		if (not (!internals)) andalso
		    (name=Extern.name_A) orelse (name=Extern.name_P)
		then L else s::L
           | (b,L) => b::L)
         alist [];

fun ppVar ppstrm (VALvar {access,name,...}) =
      (pps ppstrm (formatQid name);
       if !internals then ppAccess ppstrm access else ())
  | ppVar ppstrm (OVLDvar {name,...}) = (ppSym ppstrm (name); 
                                         pps ppstrm " : overloaded")
  | ppVar ppstrm (ERRORvar) = pps ppstrm "<ERRORvar>"

fun ppDebugVar ppstrm env  = 
   let val {begin_block,end_block,pps,...} = en_pp ppstrm
       val ppAccess = ppAccess ppstrm
       fun ppDV(VALvar {access,name,typ}) = 
            (begin_block CONSISTENT 0;
             pps "VALvar";
             begin_block CONSISTENT 3;
             pps "({access="; ppAccess access; add_comma_nl ppstrm;
             pps "name="; pps (formatQid name); add_comma_nl ppstrm;
             pps "typ=ref "; ppType env ppstrm (!typ); 
             pps "})";
             end_block(); end_block())
         | ppDV (OVLDvar {name,options,scheme}) = 
             (begin_block CONSISTENT 0;
              pps "OVLDvar";
              begin_block CONSISTENT 3;
              pps "({name="; ppSym ppstrm (name); add_comma_nl ppstrm;
              pps "options=["; 
              (ppvseq ppstrm 0 ","
	       (fn ppstrm => fn {indicator,variant} =>
	          (pps "{indicator=";ppType env ppstrm  indicator; 
                   add_comma_nl ppstrm;
	           pps " variant ="; ppDebugVar ppstrm env variant; pps "}"))
	       (!options));
              pps "]"; add_comma_nl ppstrm;
              pps "scheme="; ppTyfun ppstrm env scheme; pps "})";
             end_block(); end_block())
         | ppDV (ERRORvar) = pps "<ERRORvar>"
   in  ppDV
   end;

fun ppStructureName ppstrm (env,str) =
    let open ModuleUtil
	val path =
	    case str
	      of SIMPLE{path,...} => path
	       | INSTANCE{path,...} => path
	       | _ => ErrorMsg.impossible "PPBasics.ppStructureName"
	fun look(a,b) = case lookSTR(env,a,b)
			  of STRvar{binding,...} => binding
     in pps ppstrm (findPath(path,str,eqOrigin,look))
    end

fun ppVariable ppstrm  =
   let val {begin_block,end_block,pps,...} = en_pp ppstrm
       fun ppV(env:Modules.env,VALvar{name,access,typ}) = 
              (begin_block CONSISTENT 0;
               pps(formatQid name);
               if !internals then ppAccess ppstrm access else ();
               pps " : "; ppType env ppstrm (!typ);
               end_block())
         | ppV (env,OVLDvar {name,options=ref optl,scheme=TYFUN{body,...}}) =
             (begin_block CONSISTENT 0;
              ppSym ppstrm (name); pps " : "; ppType env ppstrm body; 
              pps " as ";
              ppSequence ppstrm
                         {sep=C PrettyPrint.add_break(1,0),
		          pr=(fn ppstrm => fn{variant,...} =>ppV(env,variant)),
		          style=CONSISTENT}
	                optl;
              end_block())
         | ppV(_,ERRORvar) = pps "<ERRORvar>"
   in  ppV
   end

and ppStructure ppstrm (topenv,str,depth) =
let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
    fun ppSig bindings =
	  (begin_block CONSISTENT 0;
	   pps "sig";
	   add_break(1,2);
	   begin_block CONSISTENT 0;
	   ppEnv ppstrm (topenv,
		         ModuleUtil.makeEnv(str,[]),
		         depth-1,bindings);
	   end_block();
	   add_break(1,0);
	   pps "end";
	   end_block())
in  case str
      of SIMPLE {stamp,env=strenv,path} =>
	    if depth <= 1 
	    then pps "..."
	    else if !internals 
		 then (begin_block CONSISTENT 0;
		       pps "SIMPLE Stamp= ";
		       pps (Stamps.stampToString stamp);
		       add_newline();
		       pps "Path= ";
		       ppSymPath ppstrm path;
		       add_newline();
		       pps "Env=";
		       add_newline();
		       ppEnv ppstrm 
                             (topenv,ModuleUtil.makeEnv(str,[]),depth-1,NONE);
		       end_block())
		 else ppSig NONE
       | INSTANCE {origin,sign,subStrs,subFcts,types,path} =>
	   (if !internals 
	    then (begin_block CONSISTENT 2;
		  pps "INSTANCE";
		  add_newline();
		  begin_block CONSISTENT 0;
		  pps "Origin=";
		  ppStructure ppstrm (topenv,origin,depth-1);
		  case sign
		    of SIG{kind=ref EMBEDDED,...} => ()
		     | _ =>
			(add_newline();
			 pps "Substructures:";
			 nl_indent ppstrm 2;
			 ppArray ppstrm 
                               (fn ppstrm => fn a => 
                                       ppStructure ppstrm 
                                                   (topenv,a,depth-1),subStrs);
			 add_newline();
			 pps "Subfunctors:";
			 nl_indent ppstrm 2;
			 ppArray ppstrm 
                                 (fn ppstrm => fn a =>
                                          ppFunctor ppstrm
                                                   (topenv,a,depth-1),subFcts);
			 add_newline();
			 pps "Types:";
			 nl_indent ppstrm 2;
			 ppArray ppstrm 
                                 (fn ppstrm => fn t => 
                                        ppTycon topenv ppstrm t,types));
		  add_newline();
		  pps "Path= ";
		  ppSymPath ppstrm path;
		  add_newline();
		  pps "Sign=";
		  ppSignature ppstrm (topenv,sign,depth-1);
		  end_block())
	    else case sign
		   of SIG{symbols,path,...} =>
		       let fun pp_str () =
			       if depth <= 1 then pps "..."
			       else ppSig (SOME(!symbols))
		       in case path
			   of SOME p =>
			       ((if ModuleUtil.eqSign
				    (sign,
				     ModuleUtil.lookSIG
				      (topenv,p, fn _ => raise Env.Unbound))
				 then ppSym ppstrm p
				 else pp_str())
				handle Env.Unbound => pp_str())
			    | NONE => pp_str()
		       end
		    | FULL_SIG => pps "<full sig>"
		    | ERROR_SIG => pps "<error>")
       | STR_FORMAL{pos,spec,...} =>
	   if depth <= 1 
	   then pps "..."
	   else if !internals then
	     (begin_block CONSISTENT 2;
	      pps "STR_FORMAL"; add_newline();
	      pps "pos="; ppi ppstrm pos; add_newline();
	      pps "spec=";
	      ppSignature ppstrm (topenv,spec,depth-1);
	      end_block())
	   else ppSignature ppstrm (topenv,spec,depth-1)
       | SELF stamp => 
	   (pps "SELF "; 
	    pps (Stamps.stampToString stamp); 
	    add_newline())
       | ERROR_STR => pps "<error>"
       | APPLY{fct,arg,res} =>
	   if depth <= 1 then pps "...\n"
	   else if !internals then
	     (begin_block CONSISTENT 2;
	      pps "Structure resulting from an application";
	      add_newline();
	      pps "fct=";
	      ppFunctor ppstrm (topenv,fct,2);
	      add_newline();
	      pps "arg=";
	      ppStructure ppstrm (topenv,arg,2);
	      add_newline();
	      pps "res=";
	      ppStructure ppstrm (topenv,res,depth-1);
	      add_newline();
	      pps "end";
	      end_block())
	   else ppStructure ppstrm (topenv,res,depth-1)
       | STR_OPEN{pos,spec,name} =>
	   if depth <= 1 then pps "...\n"
	   else if !internals then
	     (begin_block CONSISTENT 2;
	      pps "OPEN FORMAL";
	      add_newline();
	      pps "pos=";
	      ppIntPath ppstrm pos;
	      add_newline();
	      pps "name=";
	      ppSymPath ppstrm name;
	      add_newline();
	      pps "spec=";
	      ppSignature ppstrm (topenv,spec,depth-1);
	      end_block())
	   else ppSignature ppstrm (topenv,spec,depth-1)
       | STR_ABSFB loc =>
	  (pps "STR_ABSFB(";
	   case loc
	     of PARAM path =>
		  (pps "PARAM "; ppIntPath ppstrm path)
	      | SEQind (index,path) =>
		  (pps "SEQ "; ppi ppstrm index; pps " "; ppIntPath ppstrm path)
	      | SEQ index =>
		  (pps "SEQ "; ppi ppstrm index);
	   pps ")";
	   add_newline())
end        
 
and ppStructureVar ppstrm (env,STRvar{name,access,binding},depth) =
   let val {begin_block,end_block,pps,add_break,...} = en_pp ppstrm
   in
   if (not (!internals)) andalso
      ((name=Extern.name_A) orelse (name=Extern.name_P))
   then ()
   else (begin_block CONSISTENT 0;
         pps "structure ";
         ppSym ppstrm name;
         if !internals then ppAccess ppstrm access else ();
         pps " :";
         add_break(1,2);
         if name=Extern.name_P andalso false then pps "<parent def>"
         else if name=Extern.name_A then pps "<argument def>"
         else ppStructure ppstrm(env,binding,depth);
         end_block())
  end

and ppSignature ppstrm (env,sign,depth) =
  let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
  in
  if depth<=0 
  then pps "..."
  else
  case sign
    of SIG {symbols,env=ref sigenv,kind,path,stamp} =>
          if !internals 
          then 
          (begin_block CONSISTENT 2;
           pps "SIG";
           add_newline();
           pps "Path = ";
           case path
             of NONE => pps "NONE"
              | SOME p => ppSym ppstrm p;
           add_newline();
           pps "Bound symbols = ";
           ppSequence ppstrm {sep=C PrettyPrint.add_string ",", 
                              pr=ppSym, style=INCONSISTENT}
		             (!symbols);
           add_newline();
           pps "Kind =";
           case !kind
	     of TOP {strcount,fctcount,typecount,slotcount,
                     sConstraints,tConstraints} =>
                 let fun ppConstraints (f,c) =
                        ppArray ppstrm 
                          (fn ppstrm => fn {internal,external} =>
                             (begin_block INCONSISTENT 4;
                              pps "Coherence:";
                              add_newline(); (* 10 spaces in *)
                              ppSequence ppstrm
                                  {sep=fn ppstrm => (PP.add_string ppstrm " =";
                                                     PP.add_break ppstrm (1,0)),
	                                  pr=ppSymPath,
					  style=INCONSISTENT}
                                         internal;
                              end_block();
                              add_newline();
                              pps "Definitional:";
                              case external
                                of NONE => ()
                                 | SOME c => f c),
                          Array.arrayoflist c)
                     fun ppDefnStructure s = ppStructure ppstrm (env,s,depth-1)
                 in nl_indent ppstrm 2; (* Now at 4 spaces in *)
	            begin_block CONSISTENT 2;
                    pps "TOP";
                    add_newline();  (* Now at 6 spaces in *)
                    pps "Strcount="; ppi ppstrm strcount;
                    add_newline();
                    pps "Fctcount="; ppi ppstrm fctcount;
                    add_newline();
                    pps "Typecount="; ppi ppstrm typecount;
                    add_newline();
                    pps "Slot count="; ppi ppstrm slotcount;
                    add_newline();
                    pps "Structure sharing constraints:";
                    add_newline();
                    ppConstraints (ppDefnStructure,sConstraints);
                    add_newline();
                    pps "Type sharing constraints:";
                    add_newline();
                    ppConstraints (ppTycon env ppstrm,tConstraints);
                    end_block()
                 end
              | IRRELEVANT => pps "IRRELEVANT"
              | EMBEDDED => pps "EMBEDDED";
                            add_newline();
                            pps "Stamp=";pps (Stamps.stampToString stamp);
                            add_newline();
                            pps "Env =";
                            add_newline();
                            ppEnv ppstrm (env,sigenv,depth-1,SOME(!symbols));
                            end_block())
          else (* not !internals *)
          (begin_block CONSISTENT 0;
	   pps "sig";
	   add_break(1,2);
	   begin_block CONSISTENT 0;
           ppEnv ppstrm (env,sigenv,depth-1,SOME(!symbols));
           let fun ppConstraints (name,f,c) =
               (* if internal=nil, then all the constraints were definitional.
                  Thus there is no point in pping anything.*)
              app (fn {internal=nil,...} => ()   
		    | {internal,external} =>
			(add_newline();
			 begin_block INCONSISTENT 2;
			 pps "sharing ";
			 pps name;
			 ppSequence ppstrm {sep=fn ppstrm => 
                                                   (PP.add_string ppstrm " =";
                                                    PP.add_break ppstrm (1,0)),
					    pr=ppSymPath,
					    style=INCONSISTENT}
			           internal;
			 case external
			   of NONE =>()
			    | SOME c => (add_break(1,0); pps "= "; f c);
			                 add_break(1,0);  (* ? -kls *)
			                 end_block()))
		  c
            in case !kind
		 of TOP {sConstraints,tConstraints,...} =>
                     (ppConstraints("",fn s => ppStructureName ppstrm (env,s),
                                    sConstraints);
                      ppConstraints("type ",ppTycon env ppstrm,tConstraints))
		  | _ => ()
           end;
           end_block();
	   add_break(1,0);
           pps "end";
	   end_block())
     | FULL_SIG => pps "<full sig>"
     | ERROR_SIG => pps "<error>"
  end

and ppFunsig ppstrm (env,sign,depth) =
  let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
  in
  if depth<=0 then pps "..."
  else case sign
	 of FSIG {paramName,argument,body,path} => 
	     if !internals
	     then (begin_block CONSISTENT 2;
		   pps " = ";
		   add_newline();
		   pps "fsig";
		   add_newline();
		   pps "param ="; ppSym ppstrm paramName;
		   add_newline();
		   pps "argument spec = ";
		   ppSignature ppstrm (env,argument,depth-1);
		   pps "body spec =";
		   ppSignature ppstrm (env,body,depth-1);
		   pps "end\n";
		   end_block())
	     else (pps "("; ppSym ppstrm paramName; pps ":<sig>) =";
                   add_break(1,0);
		   ppSignature ppstrm (env,body,depth-1))
     | FULL_FSIG => pps "<full fsig>"
     | ERROR_FSIG => pps "<error fsig>"
  end

(* assumes no newline is needed before pping *)

and ppSignatureVar ppstrm (env:Modules.env,SIGvar{name,binding},depth) =
    let val {begin_block,end_block,pps,add_break,...} = en_pp ppstrm
    in
    (begin_block CONSISTENT 0;
     pps "signature "; ppSym ppstrm name; pps " =";
     add_break(1,2);
     ppSignature ppstrm (env,binding,depth);
     end_block())
    end

and ppFunsigVar ppstrm (env:Modules.env,FSIGvar{name,binding},depth) =
    let val {begin_block,end_block,pps,...} = en_pp ppstrm
    in
    (begin_block CONSISTENT 2;
     pps "funsig "; ppSym ppstrm name; 
     ppFunsig ppstrm (env,binding,depth);
     end_block())
    end

and ppFunctor ppstrm =
 let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
     fun ppF (env,FCT{paramName,argument,parent,
                      body={tyseq,fctseq,strseq,str,fullstr},stamp},depth) =
          if depth <= 1 
          then pps "..."
          else(begin_block CONSISTENT 0;
               pps "paramName = "; pps (Symbol.name paramName);
               add_newline();
               pps "parent:";
               nl_indent ppstrm 2;
               ppStructure ppstrm (env,parent,1);
               add_newline();
               pps "argument signature:";
               nl_indent ppstrm 2;
               ppSignature ppstrm (env,argument,depth-1);
               add_newline();
               pps "type sequence:";
               nl_indent ppstrm 2;
               ppArray ppstrm (fn ppstrm => fn t => ppTycon env ppstrm t,
                               Array.arrayoflist tyseq);
               add_newline();
               pps "structure sequence:";
               nl_indent ppstrm 2;
               ppArray ppstrm (fn ppstrm => fn s =>
                                   ppStructure ppstrm (env,s,depth-1),
                                                     Array.arrayoflist strseq);
               add_newline();
               pps "str:";
               nl_indent ppstrm 2;
               ppStructure ppstrm (env,str,depth-1))	     
       | ppF (topenv,FCT_FORMAL{pos,spec,...},depth) =
          if depth <= 1 then pps "...\n"
          else if !internals 
               then(begin_block CONSISTENT 0;
	            pps "FCT_FORMAL";
	            add_newline();
	            pps "pos="; ppi ppstrm pos;
	            add_newline();
	            pps "spec="; ppFunsig ppstrm (topenv,spec,depth-1);
	            end_block())
               else ppFunsig ppstrm (topenv,spec,depth-1)
       | ppF (topenv,FCT_OPEN{pos,spec,name},depth) =
          if depth <= 1 then pps "...\n"
          else if !internals 
               then (begin_block CONSISTENT 0;
	             pps "FCT OPEN FORMAL";
	             add_newline();
	             pps "pos="; ppIntPath ppstrm pos;
	             add_newline();
	             pps "name="; ppSymPath ppstrm name;
	             add_newline();
	             pps "spec="; ppFunsig ppstrm (topenv,spec,depth-1);
	             end_block())
               else ppFunsig ppstrm (topenv,spec,depth-1)
       | ppF (topenv,FCT_ABSFB loc,depth) =
          (pps "FCT_ABSFB(";
           case loc
	    of PARAM path => (pps "PARAM "; ppIntPath ppstrm path)
	     | SEQind (index,path) => (pps "SEQ "; ppi ppstrm index; pps " "; 
                                       ppIntPath ppstrm path)
	     | SEQ index => (pps "SEQ "; ppi ppstrm index);
           pps ")";
           add_newline())
       | ppF (topenv,FCT_INSTANCE{fsig,fct,parent},depth) =
          if !internals 
          then if depth <= 1 
               then pps "...\n"
	       else (begin_block CONSISTENT 0;
	             pps "FCT_INSTANCE";
	             nl_indent ppstrm 2;
	             pps "functor=";
	             ppF (topenv,fct,depth-1);
	             nl_indent ppstrm 2;
	             pps "constraint=";
	             ppFunsig ppstrm (topenv,fsig,depth-1);
	             end_block())
          else 
	  let val path = case fsig of FSIG{path,...} => path | _ => NONE
	     fun f() = if depth <= 1 then pps "...\n"
		       else ppFunsig ppstrm (topenv,fsig,depth-1)
	  in case path
	       of SOME p =>
		 ((if ModuleUtil.eqFsig
                      (fsig,
		       ModuleUtil.lookFSIG(topenv,p,fn _ => raise Env.Unbound))
		   then (ppSym ppstrm p; add_newline())
		   else f()) handle Env.Unbound => f())
	        | NONE => f()
	  end
       | ppF (_,ERROR_FCT,_) = pps "<error functor>"
  in  ppF
  end

(* assumes no newline is needed before pping "functor ..." *)

and ppFunctorVar ppstrm (env,FCTvar{name,access,binding},depth) =
 let val {begin_block,end_block,pps,...} = en_pp ppstrm
 in  (begin_block CONSISTENT 0;
      pps "functor ";  pps(Symbol.name name);
      if !internals then
	(pps " = "; nl_indent ppstrm 2;
	 pps "access= "; ppAccess ppstrm access;
	 nl_indent ppstrm 2;
	 pps "binding="; nl_indent ppstrm 4;
	 ppFunctor ppstrm (env,binding,depth-1))
      else pps " : <sig>";
      end_block())
 end

and ppTycBind ppstrm (env,tyc) =
    let val {begin_block,end_block,pps,add_newline,...} = en_pp ppstrm
        fun visibleDcons(tyc,dcons) =
	    let fun checkCON(CONbind c) = c
		  | checkCON _ = raise Env.Unbound
		fun find ((actual as DATACON{name,...}) :: rest) =
		     (let val found = 
			      checkCON(ModuleUtil.lookVARCON
					(env,[name],
					 fn _ => raise Env.Unbound))
		       in if TypesUtil.eqTycon(dconTyc actual,dconTyc found)
		          then actual :: find rest
		          else find rest
		      end
		      handle Env.Unbound => find rest)
		  | find [] = []
	     in find dcons
	    end
	val tyc' = case tyc
		     of FORMtyc{spec,...} => spec
		      | _ => tyc        
	fun showit(arity, name) =
	    (if EqTypes.isEqTycon tyc'
	     then pps "eqtype" 
	     else pps "type";
	     ppFormals ppstrm arity; 
	     pps " ";
	     ppSym ppstrm name)
     in begin_block CONSISTENT 2;
	if !internals 
	then (pps "type "; ppTycon env ppstrm tyc)
        else (case tyc'
		of GENtyc{path=name::_,arity,
			  kind=ref(DATAtyc dcons),...} =>
		    (case visibleDcons(tyc',dcons)
		       of [] => showit(arity,name)
			| visdcons =>
			  (pps "datatype";
			   ppFormals ppstrm arity;
			   pps " ";
			   ppSym ppstrm name; 
			   add_newline();
			   nl_app ppstrm 
                                  (fn ppstrm => fn DATACON{name,typ,...} => 
				      (pps "con ";
				       ppSym ppstrm name; 
				       pps " : ";
				       ppType env ppstrm typ))
				   visdcons;
			   if length visdcons < length dcons
			   then (add_newline();
				 pps "   ... hidden cons")
			   else ()))
		| GENtyc{path=name::_,arity,...} => showit(arity,name)
		| DEFtyc{path=name::_,tyfun=TYFUN{arity,...},...} =>
		    showit(arity,name)
		| tycon =>
		   (pps "bogus tycon: ";
		    ppTycon env ppstrm tycon;
		    ErrorMsg.impossible "PPBas.ppBinder"));
        end_block()
    end

and ppBinding ppstrm (env:Modules.env,binding,depth) =
    case binding
      of VARbind var => (pps ppstrm "val "; ppVariable ppstrm (env,var))
       | CONbind con => ppConBinding ppstrm (con,env)
       | TYCbind tycon => ppTycBind ppstrm(env,tycon)
       | SIGbind binding => ppSignatureVar ppstrm (env,binding,depth)
       | FSIGbind binding => ppFunsigVar ppstrm (env,binding,depth)
       | STRbind binding => ppStructureVar ppstrm (env,binding,depth)
       | FCTbind binding => ppFunctorVar ppstrm (env,binding,depth)
       | FIXbind(FIXvar{name,binding}) =>
	  (pps ppstrm (Fixity.fixityToString binding); ppSym ppstrm name)

(* ppEnv: pp an environment in the context of the top environment.
   The environment must either be for a signature or be absolute (i.e.
   all types and structures have been interpreted) *)
(* Note: I make a preliminary pass over bindings to remove
         invisible ConBindings -- Konrad.
	 and invisible structures too -- PC *)
and ppEnv ppstrm (topenv,env,depth,boundsyms) =
    let val bindings = 
	   case boundsyms
	     of NONE => map #2 (ModuleUtil.sortEnvBindings env)
	      | SOME l => map (fn x => Env.look(env,x)) l
	val pp_env = Env.atop(env,topenv)
     in ppSequence ppstrm
                   {sep=add_newline,
		    pr=(fn ppstrm => fn binding => ppBinding ppstrm 
                                                       (pp_env,binding,depth)),
		    style=CONSISTENT}
	(all_ppable_bindings bindings pp_env)
    end

end (* PPBasics *)
