(* Copyright 1991 by AT&T Bell Laboratories *)

signature PPTYPE = sig
  val typeFormals : int -> string list
  val tyvar_printname : Types.tyvar -> string
  val ppTycon : Modules.env -> PrettyPrint.ppstream -> Types.tycon -> unit
  val ppType : Modules.env -> PrettyPrint.ppstream -> Types.ty -> unit
  val resetPPType : unit -> unit
end

structure PPType : PPTYPE =
struct

open ErrorMsg Modules Types List2 PPUtil
structure PP = PrettyPrint

val pps = PP.add_string
fun en_pp ppstrm = {begin_block=PP.begin_block ppstrm, 
                    end_block=fn () => PP.end_block ppstrm, 
                    pps=PP.add_string ppstrm, 
                    add_break=PP.add_break ppstrm, 
                    add_newline=fn () => PP.add_newline ppstrm};
fun C f x y = f y x;

val internals = System.Control.internals

fun make_tyvar_printname (eq,weakness,depth) basename =
    implode [
      (if eq then "''" else "'"),
      (if weakness < infinity then
	 (if !System.Control.weakUnderscore then
	      "_"
	  else (makestring weakness))
       else ""),
      basename,
      (if !internals andalso depth < infinity then
	 implode ["[",(makestring depth),"]"]
       else "")]

fun boundTyvarName k =
    if k < 26
    then chr(k+ord"a")
    else chr(k quot 26 + ord "a") ^ chr(k rem 26 + ord"a")

fun metaTyvarName' k =
    if k < 26
    then chr(25-k+ord "A")
    else chr(25-(k quot 26) + ord "A") ^ chr(25-(k rem 26) + ord"A")

fun typeFormals n =
    let fun loop i =
	if i>=n then []
	else (boundTyvarName i)::loop(i+1)
     in loop 0
    end

local  
  val count = ref(~1)
  val metaTyvars = ref([]:tyvar list)
in
  fun metaTyvarName(tv: tyvar) =
      let fun find([],_) =
	        (metaTyvars := tv::(!metaTyvars);
		 inc count;
		 !count)
	    | find(tv'::rest,k) =
	        if tv = tv'
		then !count - k
		else find(rest,k+1)
       in metaTyvarName' (find(!metaTyvars,0))
      end
  fun resetPPType() = (count := ~1; metaTyvars := [])
end

fun make_tyvar_basename (tyvar as ref(OPEN{kind=META,...})) =
				metaTyvarName tyvar
  | make_tyvar_basename (ref(OPEN{kind=UBOUND name,...})) =
				(Symbol.name name) ^ "U"
  | make_tyvar_basename (tyvar as ref(OPEN{kind=FLEX _,...})) =
				"..." ^ (metaTyvarName tyvar)
  | make_tyvar_basename _ = impossible "make_tyvar_basename"

fun tyvar_printname (ref(INSTANTIATED(VARty(tyvar)))) = tyvar_printname tyvar
  | tyvar_printname (tyvar as ref(OPEN{depth,weakness,eq,...})) =
        make_tyvar_printname (eq,weakness,depth) (make_tyvar_basename tyvar)
  | tyvar_printname _ = impossible "tyvar_printname"

fun ppkind ppstrm kind =
   pps ppstrm (case kind
                 of PRIMtyc => "PRIM"
	          | FORMtyck => "FORM"
                  | ABStyc _=> "ABS"
                  | DATAtyc _ => "DATA")

fun ppPath env ppstrm (p,tyc0) =
    pps ppstrm (ModuleUtil.findPath(p,tyc0,TypesUtil.equalTycon,
                                    fn (a,b) => ModuleUtil.lookTYC(env,a,b)))

val GENtyc{stamp=arrowStamp,...} = BasicTypes.arrowTycon

fun strength(ty) =
    case ty
      of VARty(ref(INSTANTIATED ty')) => strength(ty')
       | CONty(tycon, args) =>
	   (case tycon
 	      of GENtyc{kind=ref(PRIMtyc), stamp,...} => 
		   if stamp = arrowStamp then 0 else 2
	       | RECORDtyc (_::_) =>  (* excepting type unit *)
		   if Tuples.isTUPLEtyc(tycon) then 1 else 2
	       | _ => 2)
       | _ => 2

fun ppEqProp ppstrm p =
    let val a = case p
		  of NO => "NO"
		   | YES => "YES"
		   | IND => "IND"
		   | OBJ => "OBJ"
		   | DATA => "DATA"
		   | UNDEF => "UNDEF"
     in pps ppstrm a
    end

fun ppTycon env ppstrm =
 let val {begin_block,end_block,pps,add_break,...} = en_pp ppstrm
     fun ppTyc (tyc as GENtyc{path,stamp,eq,kind,...}) =
	 (if !internals
	  then (begin_block PP.INCONSISTENT 1;
		pps "GENtyc{path=";
		ppSymPath ppstrm path;
		pps ",";
		add_break(0,0);

		pps "kind=";
		ppkind ppstrm (!kind);
		pps ",";
		add_break(0,0);

		pps "stamp=";
		pps (Stamps.stampToString stamp);
		pps ",";
		pps "eq="; ppEqProp ppstrm (!eq);
		pps "}";
		end_block())
	  else ppPath env ppstrm (path,tyc))
       | ppTyc(tyc as DEFtyc{path,tyfun=TYFUN{body,...},...}) =
	  if !internals
	  then (begin_block PP.INCONSISTENT 7;
		pps "DEFtyc{path=";
		ppSymPath ppstrm path;
		pps ",";
		add_break(0,0);

		pps "tyfun=TYFUN{body=";
		ppType env ppstrm body;
		pps ",";
		add_break(0,0);
		pps "...}}";
		end_block())
	  else ppPath env ppstrm (path,tyc)
      | ppTyc(RECORDtyc labels) =
	  ppClosedSequence ppstrm 
	    {front=C PP.add_string "{",
	     sep=fn ppstrm => (PP.add_string ppstrm ","; 
                               PP.add_break ppstrm (0,0)),
	     back=C PP.add_string "}",
	     style=PP.INCONSISTENT,
	     pr=ppSym} labels
      | ppTyc(FORMtyc {pos,spec,name}) =
	 (begin_block PP.INCONSISTENT 8;
	  pps "FORMtyc{pos="; pps (makestring pos);
	  pps ",";
	  add_break(0,0);
	  pps "spec="; ppTycon env ppstrm spec;
	  pps ",";
	  add_break(0,0);
	  pps ",name="; ppSym ppstrm name;
	  pps "}";
	  end_block())
      | ppTyc(OPENFORMtyc {pos=(p,i),spec,name}) =
	 (pps "OPENFORMtyc{pos=(";
	  ppIntPath ppstrm p;
	  pps ",";
	  add_break(0,0);
	  pps (makestring i);
	  pps ")";
	  pps ",";
	  add_break(0,0);
	  pps "name="; ppSymPath ppstrm name;
	  pps ",";
	  add_break(0,0);
	  pps "spec="; ppTycon env ppstrm spec; 
	  pps "}";
	  end_block())
      | ppTyc(RELtyc {name,pos=(p,i)}) =
	 (if !internals
	  then (begin_block PP.INCONSISTENT 8;
		pps "RELtyc{name=";
		ppSymPath ppstrm name;
		pps ",";
		add_break(0,0);

		pps "pos=("; ppIntPath ppstrm p;
		pps ",";
		add_break(0,0);
		pps (makestring i);
		pps ")}";
		end_block())
	  else ();
	  pps (formatQid name))
      | ppTyc(ABSFBtyc pos) =
	 (begin_block PP.INCONSISTENT 3;
	  pps "ABSFBtyc";
	  add_break(0,0);
	  pps"(";
	  case pos
	    of PARAM p => (pps "PARAM "; ppIntPath ppstrm p)
	     | SEQind (i,p) => (pps "SEQind ";pps(makestring i);pps " ";
                                ppIntPath ppstrm p)
	     | SEQ i => (pps "SEQ "; pps (makestring i));
	  pps ")";
	  end_block())
      | ppTyc ERRORtyc = pps "<error>"
      | ppTyc FULLtyc = pps "<full tyc>"
  in  ppTyc
  end


and ppType1 env ppstrm (ty: ty, sign: {weakness:int,eq:bool} list) : unit =
    let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
        fun prty ty =
	    case ty
	      of VARty(ref(INSTANTIATED ty')) => prty(ty')
	       | VARty(tv) => ppTyvar tv
	       | IBOUND n =>
		   let val {weakness,eq} = nth(sign,n) 
		            handle Nth => {weakness=infinity,eq=false}
		    in pps (make_tyvar_printname(eq,weakness,infinity)
			    (boundTyvarName n))
		   end
	       | CONty(tycon, args) =>
		   (case tycon
		      of GENtyc{kind=ref(PRIMtyc), stamp,...} => 
			   if stamp = arrowStamp
			   then case args
			         of [domain,range] =>
				    (begin_block PP.CONSISTENT 0;
				     if strength domain = 0
				     then (begin_block PP.CONSISTENT 1;
					   pps "(";
					   prty domain;
					   pps ")";
					   end_block())
				     else prty domain;
				     add_break(1,0);
				     pps "-> ";
				     prty range;
				     end_block())
				  | _ =>impossible "CONty:arity"
			   else (begin_block PP.INCONSISTENT 2;
                                 ppTypeArgs args;
                                 add_break(0,0);
                                 ppTycon env ppstrm tycon;
                                 end_block())
		       | RECORDtyc labels =>
			   if Tuples.isTUPLEtyc(tycon)
			   then ppTUPLEty args
			   else ppRECORDty(labels, args)
		       | _ =>
			   (begin_block PP.INCONSISTENT 2;
			    ppTypeArgs args; 
			    add_break(0,0);
			    ppTycon env ppstrm tycon;
			    end_block()))
	       | POLYty{sign,abs,tyfun=TYFUN{arity,body}} => 
                        ppType1 env ppstrm (body,sign)
	       | WILDCARDty => pps "_"
	       | UNDEFty => pps "undef"

	and ppTypeArgs [] = ()
	  | ppTypeArgs [ty] = 
	     (if strength ty <= 1
	      then (begin_block PP.INCONSISTENT 1;
                    pps "("; 
                    prty ty; 
                    pps ")";
                    end_block())
	      else prty ty;
	      add_break(1,0))
	  | ppTypeArgs tys =
              ppClosedSequence ppstrm 
	        {front=C PP.add_string "(",
		 sep=fn ppstrm => (PP.add_string ppstrm ",";
                                   PP.add_break ppstrm (0,0)),
		 back=C PP.add_string ") ",
		 style=PP.INCONSISTENT, 
                 pr=fn _ => fn ty => prty ty}
		tys
	and ppTUPLEty [] = pps "unit"
	  | ppTUPLEty tys = 
	      ppSequence ppstrm
                         {sep=fn ppstrm => (PP.add_break ppstrm (1,0);
                                            PP.add_string ppstrm "* "),
			  style=PP.INCONSISTENT,
			  pr=(fn _ => fn ty =>
			       if strength ty <= 1
			       then (begin_block PP.INCONSISTENT 1;
				     pps "("; 
				     prty ty; 
				     pps ")";
				     end_block())
			       else prty ty)}
	        tys

	and ppField(lab,ty) = (begin_block PP.CONSISTENT 0;
			       ppSym ppstrm lab; 
			       pps ":";
			       prty ty;
			       end_block())

	and ppRECORDty([],[]) = pps "unit"
	  | ppRECORDty(lab::labels, arg::args) =
	      (begin_block PP.INCONSISTENT 1;
               pps "{";
               ppField(lab,arg);
	       List2.app2 
		 (fn field => (pps ","; add_break(1,0);ppField field))
		 (labels,args);
               pps "}";
               end_block())
	  | ppRECORDty _ = ErrorMsg.impossible "PPType.ppRECORDty"

	and ppTyvar (tv as (ref (OPEN{depth,weakness,eq,kind})) :tyvar) :unit =
	      let val printname = tyvar_printname tv
	       in case kind
		    of FLEX fields =>
			(case fields
			   of [] => (pps "{"; pps printname; pps "}")
			    | field::fields =>
				(begin_block PP.INCONSISTENT 1;
				 pps "{";
				 ppField field;
				 app (fn x => (pps ",";
					       add_break(1,0);
					       ppField x))
				      fields;
				 pps ",";
				 add_break(1,0);
				 pps printname;
				 pps "}";
				 end_block()))
		     | _ => pps printname
	      end
	  | ppTyvar _ = impossible "printTyvar"

     in prty ty
    end  (* ppType1 *)

and ppType (env:Modules.env) ppstrm (ty:ty) : unit = 
      (PP.begin_block ppstrm PP.INCONSISTENT 1;
       ppType1 env ppstrm (ty,[]);
       PP.end_block ppstrm)

end (* structure PPType *)
