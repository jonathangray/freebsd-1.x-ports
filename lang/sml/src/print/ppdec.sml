(* Copyright 1989 by AT&T Bell Laboratories *)
(* ppdec.sml *)

structure PPDec : PPDEC =
struct 

open Types Variables Modules Fixity Absyn
     PrettyPrint PPUtil PPType PPVal PPBasics Access

type object = System.Unsafe.object

val signatures = System.Print.signatures
val printDepth = System.Print.printDepth

fun pplist_nl ppstrm pr =
   let fun pp [] = ()
         | pp [el] = pr el
         | pp (el::rst) = (pr el; add_newline ppstrm; pp rst)
   in  pp
   end;

fun C f x y = f y x;

fun ppDec env ppstrm dec lookup =
    let val dec = (* pruneDec *) dec
        fun ppVb (VB{pat,...}) =
	    let fun ppBind(pat) =
		    case pat
		      of VARpat(VALvar{name=[n],access,typ=ref ty}) => 
			   (begin_block ppstrm CONSISTENT 0;
			    begin_block ppstrm INCONSISTENT 2;
			    add_string ppstrm "val "; 
			    ppSym ppstrm n; 
			    add_string ppstrm " =";
			    add_break ppstrm (1,0);
			    case access
			      of PATH[lv] => ppVal env ppstrm 
                                                   (lookup lv, ty, !printDepth)
			       | INLINE _ => add_string ppstrm "<primop>"
			       | _ => ErrorMsg.impossible
				        "ppDec.ppVb.ppBind.VARpat";
			    add_break ppstrm (1,0);
			    add_string ppstrm ": "; 
			    ppType env ppstrm ty;
			    end_block ppstrm;
			    add_newline ppstrm;
			    end_block ppstrm)
		       | RECORDpat{pats=ref pl,...} => app ppBind pl
		       | VECTORpat(pats,_) => app ppBind pats
		       | APPpat(_,_,pat) => ppBind pat
		       | CONSTRAINTpat(pat,_) => ppBind pat
		       | LAYEREDpat(pat1,pat2) => (ppBind pat1; ppBind pat2)
		       | _ => ()
	     in ppBind pat
	    end

	and ppRvb (RVB{var=VALvar{name=[n],access=PATH[lv],typ},...}) = 
	    (begin_block ppstrm CONSISTENT 0;
	     begin_block ppstrm INCONSISTENT 2;
	     add_string ppstrm "val "; 
	     ppSym ppstrm n; 
	     add_string ppstrm " =";
	     add_break ppstrm (1,0);
                                (* only prints functions! *)
	     ppVal env ppstrm (lookup lv, !typ, !printDepth);
	     add_break ppstrm (1,0);
	     add_string ppstrm ": "; 
	     ppType env ppstrm (!typ); 
	     end_block ppstrm;
	     add_newline ppstrm;
	     end_block ppstrm)

	and ppTb(TB{tyc=DEFtyc{path=name::_,tyfun=TYFUN{arity,...},...},def}) =
	    (begin_block ppstrm CONSISTENT 0;
	     begin_block ppstrm INCONSISTENT 2;
	     add_string ppstrm "type "; 
	     ppFormals ppstrm arity; 
	     add_break ppstrm (1,0);
	     ppSym ppstrm name; 
	     add_string ppstrm " ="; 
	     add_break ppstrm (1,0);
	     ppType env ppstrm def;
	     end_block ppstrm;
             add_newline ppstrm;
	     end_block ppstrm)

	and ppAbsTyc(GENtyc{path=name::_, arity,eq,kind=ref(ABStyc _), ...}) =
	    (begin_block ppstrm CONSISTENT 0;
	     begin_block ppstrm INCONSISTENT 2;
	     add_string ppstrm(if (!eq=YES) then "eqtype" else "type"); 
	     ppFormals ppstrm arity; 
	     add_break ppstrm (1,0);
	     ppSym ppstrm name; 
	     end_block ppstrm;
             add_newline ppstrm;
	     end_block ppstrm)

	and ppDataTyc(GENtyc{path=name::_,arity,kind=ref(DATAtyc dcons),...}) =
	    (begin_block ppstrm CONSISTENT 0;
	     begin_block ppstrm CONSISTENT 0;
	     add_string ppstrm "datatype ";
	     ppFormals ppstrm arity;
	     add_string ppstrm " ";
	     ppSym ppstrm name; 
             add_break ppstrm (500,2); (* force a linebreak with indent 2 *)
	     begin_block ppstrm CONSISTENT 0;
	     pplist_nl ppstrm
	       (fn DATACON{name,typ,...} => 
		     (begin_block ppstrm INCONSISTENT 0;
		      add_string ppstrm "con "; 
		      ppSym ppstrm name; 
		      add_break ppstrm (1,0); 
		      add_string ppstrm ": "; 
		      ppType env ppstrm typ;
		      end_block ppstrm))
		 dcons;
	     end_block ppstrm;
	     end_block ppstrm;
	     add_newline ppstrm;
	     end_block ppstrm)

	and ppEb(EBgen{exn=DATACON{name,...},etype,...}) =
	      (begin_block ppstrm CONSISTENT 0;
	       begin_block ppstrm INCONSISTENT 2;
	       add_string ppstrm "exception "; 
	       ppSym ppstrm name;
	       case etype
		 of NONE => ()
		  | SOME ty' => 
		           (add_string ppstrm " of"; 
			    add_break ppstrm (1,0);
			    ppType env ppstrm ty');
	       end_block ppstrm;
 	       add_newline ppstrm;
	       end_block ppstrm)
	  | ppEb(EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =
	      (begin_block ppstrm CONSISTENT 0;
	       begin_block ppstrm INCONSISTENT 2;
	       add_string ppstrm "exception "; 
	       ppSym ppstrm name;
	       add_string ppstrm " ="; 
	       add_break ppstrm (1,0);
	       ppSym ppstrm dname;
	       end_block ppstrm;
 	       add_newline ppstrm;
	       end_block ppstrm)

	and ppStrb isAbs (STRB{strvar,...}) =    (* isAbs strvar *)
	    (begin_block ppstrm CONSISTENT 0;
	     PPBasics.ppStructureVar ppstrm (env,strvar,!signatures);
	     add_newline ppstrm;
	     end_block ppstrm)

	and ppFctb(FCTB{fctvar,...}) = 
	    (begin_block ppstrm CONSISTENT 0;
	     PPBasics.ppFunctorVar ppstrm (env,fctvar,!signatures);
	     add_newline ppstrm;
	     end_block ppstrm)

        and ppSigb s = 
	    (begin_block ppstrm CONSISTENT 0;
	     PPBasics.ppSignatureVar ppstrm (env,s,!signatures);
	     add_newline ppstrm;
	     end_block ppstrm)

        and ppFsigb s = 
	    (begin_block ppstrm CONSISTENT 0;
	     PPBasics.ppFunsigVar ppstrm (env,s,!signatures);
	     add_newline ppstrm;
	     end_block ppstrm)

	and ppFixity{fixity,ops} =
	    (begin_block ppstrm CONSISTENT 0;
	     begin_block ppstrm CONSISTENT 0;
	     add_string ppstrm (Fixity.fixityToString fixity);
	     PPUtil.ppSequence ppstrm {sep=C PrettyPrint.add_break (1,0),
			               pr=PPUtil.ppSym,
			               style=INCONSISTENT}
	                       ops;
	     end_block ppstrm;
	     add_newline ppstrm;		       
	     end_block ppstrm)

	and ppOpen(strvl) =  
	    (begin_block ppstrm CONSISTENT 0;
	     begin_block ppstrm CONSISTENT 0;
	     add_string ppstrm "open ";
	     ppSequence ppstrm {sep=C PrettyPrint.add_break (1,0),
			pr=(fn ppstrm => fn STRvar{name,...}
                            => ppSym ppstrm name),
			style=INCONSISTENT}
	                strvl;
	     end_block ppstrm;
	     add_newline ppstrm;		       
	     end_block ppstrm)

	and ppDec0 dec =
	    case (resetPPType(); dec)
	      of VALdec vbs => app ppVb vbs
	       | VALRECdec rvbs => app ppRvb rvbs
	       | TYPEdec tbs => app ppTb tbs
	       | DATATYPEdec{datatycs,withtycs} =>
		   (app ppDataTyc datatycs; 
		    app ppTb withtycs)
	       | ABSTYPEdec{abstycs,withtycs,body} =>
		   (app ppAbsTyc abstycs;
		    app ppTb withtycs;
		    ppDec0 body)
	       | EXCEPTIONdec ebs => app ppEb ebs
	       | STRdec strbs => app (ppStrb false) strbs
	       | ABSdec strbs => app (ppStrb true) strbs
	       | FCTdec fctbs => app ppFctb fctbs
	       | SIGdec sigvars => app ppSigb sigvars
	       | FSIGdec sigvars => app ppFsigb sigvars
	       | LOCALdec(decIn,decOut) => ppDec0 decOut
	       | SEQdec decs => app ppDec0 decs
	       | FIXdec fixd => ppFixity fixd
	       | OVLDdec _ => (add_string ppstrm "overload"; add_newline ppstrm)
	       | OPENdec strvs => ppOpen strvs
	       | MARKdec(dec,a,b) => ppDec0 dec

     in begin_block ppstrm CONSISTENT 0;
	ppDec0 dec;
	end_block ppstrm;
	flush_ppstream ppstrm
    end

end (* structure PPDec *)
