(* Copyright 1989 by AT&T Bell Laboratories *)
(* ppval.sml *)

structure PPVal : PPVAL =

struct
open List

structure PP = PrettyPrint

open Types Fixity Access Modules
open System System.Unsafe BasicTypes TypesUtil ErrorMsg PrettyPrint PPUtil

fun gettag obj = int (Vector.sub(tuple obj,0))

exception Switch

fun switch(obj, GENtyc{kind=ref(DATAtyc dcons),...}) =
    let fun try ((d as DATACON{rep,...})::r) = 
	    (case rep
	       of TAGGED i => if (gettag obj = i handle Boxity => false)
			      then d else try r
		| TAGGEDREC(i,_) => if (gettag obj = i handle Boxity => false)
			      then d else try r
		| CONSTANT i => if (int obj = i handle Boxity => false)
				then d else try r
		| TRANSPARENT =>
			    if ((tuple obj; true) handle Boxity => false)
			    then d else (try r handle Switch => d)
		| UNTAGGED => if ((tuple obj; true) handle Boxity => false)
			    then d else try r
		| UNTAGGEDREC _ => if ((tuple obj;true) handle Boxity => false)
			    then d else try r
		| REF => d
		| _ => ErrorMsg.impossible "PPVal.switch: funny datacon")
	  | try nil = raise Switch
    in  try dcons
	handle Switch => 
	  ErrorMsg.impossible "PPVal.switch: none of the datacons matched"
    end

fun decon(obj, DATACON{rep,...}) =
    case rep
     of UNTAGGED => Vector.sub(tuple obj,0)
      | TAGGED _ => Vector.sub(tuple obj,1)
      | TAGGEDREC _ =>
	  let (* skip first element, i.e. discard tag *)
	      val a = tuple obj
	      fun f i =
		if (i < Vector.length a) then Vector.sub(a,i) :: f(i+1) else []
	   in cast (Vector.vector (f(1)))
	  end
      | CONSTANT _ => ErrorMsg.impossible "PPVal.decon - constant datacon in decon"
      | TRANSPARENT => obj
      | UNTAGGEDREC _ => obj
      | REF => Vector.sub(tuple obj,0)
      | VARIABLE _ => Vector.sub(tuple obj,0)
      | _ => ErrorMsg.impossible "PPVal.decon - unexpected conrep"

val noparen = INfix(0,0)

fun is_relative(VARty(ref(INSTANTIATED ty))) = is_relative(ty)
  | is_relative(CONty(RELtyc _, _)) = true
  | is_relative(CONty(_,args)) = exists is_relative args
  | is_relative _ = false

local
  (* counter to generate identifier *)
  val cpt = ref 0

  (* test membership in an association list and gives back 
   * the second element *)
  fun mem a =
    let fun m [] = NONE | m ((x,r)::l) = if a = x then SOME r else m l
    in m end

  (* verifies if an object has been seen and if yes, gives back its
   * identification number, creating a new one if necessary *)
  fun isSeen obj l =
    let val obj' = System.Unsafe.cast obj : unit ref
    in
    case mem obj' l 
    of NONE => (false,0)
     | SOME (r as ref NONE) => 
	 let val id = !cpt before inc cpt in (r := SOME id; (true,id)) end
     | SOME (ref (SOME id)) => (true,id)
    end

in
(* reset the identifier counter *)
fun initCpt () = cpt := 0

(* print with sharing if necessary. The "printer" already knows the 
   ppstream.      *)
fun printWithSharing ppstrm (obj,accu,printer) = 
  if !System.Print.printLoop then
    let val (seen,nb) = isSeen obj accu
    in 
    if seen then (PP.add_string ppstrm "%";
                  PP.add_string ppstrm (makestring nb))
    else
      let val modif = ref NONE
	  val nlAccu = (System.Unsafe.cast obj : unit ref,modif) :: accu
      in
      printer (obj,nlAccu);
      case !modif 
      of NONE => () 
       | SOME i => (PP.add_string ppstrm " as %";
                    PP.add_string ppstrm (makestring i))
      end
    end
  else printer (obj,accu)
end

fun ppVal env ppstrm =
let fun ppValue (obj: object, ty: ty, depth: int) : unit =
        ppVal' (obj, ty, depth, noparen, noparen,[])
    and ppValShare (obj:object, ty:ty, depth:int,accu) =
        ppVal' (obj, ty, depth, noparen, noparen,accu)
    and ppVal' (_,_,0,_,_,_) = add_string ppstrm  "#"
      | ppVal' (obj: object,ty: ty, depth: int,l: fixity,r: fixity,accu) : unit =
    case ty
      of VARty(ref(INSTANTIATED t)) => ppVal'(obj,t,depth,r,l,accu)
       | CONty(tyc as GENtyc{kind=ref(PRIMtyc),...}, argtys) =>
	   if eqTycon(tyc,intTycon) then add_string ppstrm (makestring(int obj))
	   else if eqTycon(tyc,realTycon) then add_string ppstrm (makestring(real obj))
	   else if eqTycon(tyc,stringTycon) then PPUtil.pp_mlstr ppstrm (string obj)
	   else if eqTycon(tyc,arrowTycon) then add_string ppstrm  "fn"
	   else if eqTycon(tyc,exnTycon) 
		    then let val name = System.exn_name(System.Unsafe.cast obj)
			  in add_string ppstrm name;
			      add_string ppstrm "(-)"
			 end
	   else if eqTycon(tyc,contTycon) then add_string ppstrm  "cont"
	   else if eqTycon(tyc,vectorTycon) then 
	     ppVector( tuple obj, hd argtys, depth,
			 !System.Print.printLength,accu,"#[","]")
	     handle Boxity => add_string ppstrm  "prim?"
	   else if eqTycon(tyc,arrayTycon) then ( 
	     printWithSharing ppstrm
	       (obj,accu,
		fn (obj,accu) =>
		     ppVector ( tuple obj, hd argtys, depth, 
			       !System.Print.printLength,
			       accu,"[|","|]"))
	     handle Boxity => add_string ppstrm  "prim?")
	   else add_string ppstrm  "prim?"
       | CONty(tyc as GENtyc{kind=ref(ABStyc _),stamp,...}, _) => 
           (PPTable.pp_object ppstrm stamp obj 
            handle PP_NOT_INSTALLED => add_string ppstrm  "-" )
       | CONty(tyc as GENtyc{kind=ref(DATAtyc _),...}, argtys) => (* wrong!? *)
    	   if eqTycon(tyc,listTycon)
    	   then ppList( obj, hd argtys, depth,
		       !System.Print.printLength,accu)
           else if eqTycon(tyc,refTycon) then
             printWithSharing ppstrm
               (obj,accu,
                fn (obj,accu) =>
                 ppDcon( obj, tyc, argtys, depth, l, r,accu))
    	   else ppDcon( obj, tyc, argtys, depth, l, r,accu)
       | CONty(tyc as RECORDtyc [], _) => add_string ppstrm  "()"
       | CONty(tyc as RECORDtyc labels, argtys) =>
	   if Tuples.isTUPLEtyc tyc
	   then ppTuple(tuple(obj), argtys, depth,accu)
	   else ppRecord(tuple(obj), labels, argtys, depth,accu)
       | CONty(tyc as DEFtyc _, _) => 
	   ppVal'(obj, reduceType ty, depth, l, r,accu)
       | POLYty{tyfun=TYFUN{body,...},...} =>
	   ppVal'(obj,body,depth,l,r,accu)
       | _ => add_string ppstrm  "-"

and ppDcon(_,_,_,0,_,_,_) = add_string ppstrm  "#"
  | ppDcon(obj:object, tyc as GENtyc{arity,stamp,...}, argtys,
	   depth:int, l:fixity, r:fixity,accu) =
    let val dcon as DATACON{name,const,typ,...} = switch(obj,tyc)
	val dname = Symbol.name name
     in PPTable.pp_object ppstrm stamp obj
	   (* attempt to find and apply user-defined pp on obj *)
        handle PP_NOT_INSTALLED => 
	 if const
	 then add_string ppstrm  dname
	 else let val fixity = ModuleUtil.lookFIX(env,Symbol.fixSymbol
                                                         (Symbol.name name))
		      (* (??) may be inaccurate *)
		  val dom = case typ
			      of CONty(_,dom::_) => dom
			       | POLYty{tyfun=TYFUN{body=CONty(_,dom::_),...},
					...} =>
				 applyTyfun(TYFUN{arity=arity,body=dom},argtys)
		  val dom = headReduceType (dom)
		  fun prdcon() =
		      case (fixity,dom)
		       of (INfix _,CONty(domTyc as RECORDtyc _, [tyL,tyR])) =>
			  let val twoTuple = tuple(decon(obj,dcon))
			  in if Tuples.isTUPLEtyc domTyc
			     then (begin_block ppstrm INCONSISTENT 0;
				   ppVal'(Vector.sub(twoTuple,0),tyL,
                                          depth-1,NONfix,fixity,accu);
				   add_break ppstrm (1,0); add_string ppstrm  dname;
				   add_break ppstrm (1,0);
				   ppVal'(Vector.sub(twoTuple,1),tyR,
					  depth-1,fixity, NONfix,accu);
				   end_block ppstrm)
			     else (begin_block ppstrm INCONSISTENT 2;
				   add_string ppstrm  dname; add_break ppstrm (1,0);
				   ppVal'(decon(obj,dcon),dom,depth-1,
                                          NONfix,NONfix,accu);
				   end_block ppstrm)
			  end
			| _ => (begin_block ppstrm INCONSISTENT 2;
			        add_string ppstrm  dname; add_break ppstrm (1,0);
			        ppVal'(decon(obj,dcon),dom,depth-1,
                                       NONfix,NONfix,accu);
			        end_block ppstrm)
                  fun prpardcon() =
		      (begin_block ppstrm INCONSISTENT 0;
		       add_string ppstrm  "("; prdcon(); add_string ppstrm  ")";
		       end_block ppstrm)
	       in case(l,r,fixity)
		   of (NONfix,NONfix,_) => prpardcon()
		    | (INfix _,INfix _,_) => prdcon()
		      (* special case: only on first iteration, for no parens *)
		    | (_,_,NONfix) => prdcon()
		    | (INfix(_,p1),_,INfix(p2,_)) =>
			if p1 >= p2 then prpardcon()
			else prdcon()
		    | (_,INfix(p1,_),INfix(_,p2)) =>
			if p1 > p2 then prpardcon()
			else prdcon()
	      end
    end

and ppList(obj:object, ty:ty, depth:int, length: int,accu) =
    let fun list_case p =
            let val dcon as DATACON{name,...} = switch(p, listTycon)
	     in case (Symbol.name name)
	          of "nil" => NONE
		   | "::" =>
		       let val pair = tuple(decon(p, dcon))
		       in  SOME(Vector.sub(pair,0),Vector.sub(pair,1))
		       end
	    end
       
       fun ppTail(p, len) =
	   case list_case p
	     of NONE => ()
	      | SOME(hd,tl) => 
		  if len <= 0 then (add_string ppstrm  "...")
		  else (case list_case tl
			 of NONE => 
			      ppValShare (hd, ty, depth-1,accu)
			  | _ =>
			      (ppValShare (hd, ty, depth-1,accu);
			       add_string ppstrm  ",";
			       add_break ppstrm (0,0);
			       ppTail(tl,len-1)))

     in begin_block ppstrm INCONSISTENT 1;
        add_string ppstrm  "["; 
        ppTail(obj,length);
	add_string ppstrm  "]";
        end_block ppstrm
    end

and ppTuple(objs: object vector, tys: ty list, depth:int, accu) : unit =
    let fun ppFields(nf,[ty]) =
	      ppValShare (Vector.sub(objs,nf),ty,depth-1,accu)
	  | ppFields(nf, ty::restty) = 
	      (ppValShare (Vector.sub(objs,nf),ty,depth-1,accu);
               add_string ppstrm (",");
               add_break ppstrm (0,0);
	       ppFields(nf+1,restty))
	  | ppFields(nf,[]) = ()
     in begin_block ppstrm INCONSISTENT 1;
        add_string ppstrm ("("); 
        ppFields(0,tys); 
        add_string ppstrm (")");
        end_block ppstrm
    end

and ppRecord(objs: object vector, labels: label list,
	     tys: ty list, depth: int, accu) =
    let fun ppFields(nf,[l],[ty]) = 
	      (begin_block ppstrm CONSISTENT 2;
               add_string ppstrm (Symbol.name l); 
               add_string ppstrm ("="); 
               ppValShare (Vector.sub(objs,nf),ty,depth-1,accu);
               end_block ppstrm)
	  | ppFields(nf, l::restl, ty::restty) = 
	      (begin_block ppstrm CONSISTENT 2;
               add_string ppstrm (Symbol.name l); 
               add_string ppstrm ("="); 
               ppValShare (Vector.sub(objs,nf),ty,depth-1,accu);
               end_block ppstrm;
	       add_string ppstrm (","); 
               add_break ppstrm (0,0);
               ppFields(nf+1,restl,restty))
	  | ppFields(nf,[],[]) = ()
     in begin_block ppstrm INCONSISTENT 1;
        add_string ppstrm ("{"); 
        ppFields(0,labels,tys); 
        add_string ppstrm ("}");
        end_block ppstrm
    end

and ppVector(objs:object vector, ty:ty, depth:int,
		length: int,accu,openSymb,closeSymb) =
      let val vectorLength  = objLength objs
          val (len, closing) = 
	        if length >= vectorLength then 
		  (vectorLength,fn _ => add_string ppstrm  closeSymb)
		else (length,fn sep => (add_string ppstrm  sep; 
                                        add_string ppstrm  ("..."^closeSymb)))
          fun printRest(sep,breaker, index) =
	        if index >= len then closing sep
                else (add_string ppstrm  sep; breaker ();
		      ppValShare (Vector.sub (objs,index),ty,depth-1,accu);
		      printRest (",",fn () => add_break ppstrm (0,0), index + 1))
      in 
      begin_block ppstrm INCONSISTENT 1;
      add_string ppstrm (openSymb:string); printRest("",fn () => (), 0);
      end_block ppstrm
      end

in ppValue
end;

end (* structure PPVal *)
