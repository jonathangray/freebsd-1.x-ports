(* genabs.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories
 *)

(* Note to browsers: You probably don't want to read this,  it's not
 part of the normal Standard ML of New Jersey implementation;
  read generic.sml instead. *)

structure GenAbs = 
struct

open AbsMach

fun realc s =
  let val (sign,s) = case explode s of "~"::rest => (~1.0,rest) | s => (1.0,s)
      fun j(exp,d::dl,mant) = j(exp,dl,mant * 0.1 + real(d))
        | j(0,nil,mant) = mant*sign
        | j(exp,nil,mant) = if exp>0 then j(exp-1,nil,mant*10.0)
				     else j(exp+1,nil,mant*0.1)
      fun h(esign,wholedigits,diglist,exp,nil) = 
			j(esign*exp+wholedigits-1,diglist,0.0)
        | h(es,fd,dl,exp,d::s) = h(es,fd,dl,exp*10+(ord d - ord "0"),s)
      fun g(i,r,"E"::"~"::s)=h(~1,i,r,0,s)
	| g(i,r,"E"::s)=h(1,i,r,0,s)
	| g(i,r,d::s) = g(i, (ord d - ord "0")::r, s)
	| g(i,r,nil) = h(1,i,r,0,nil)
      fun f(i,r,"."::s)=g(i,r,s)
        | f(i,r,s as "E"::_)=g(i,r,s)
        | f(i,r,d::s) = f(i+1,(ord(d)-ord("0"))::r,s)
   in f(0,nil,s)
  end

val zeroreg = 0
val allocptr = 1
val exnhandler = 2
val standardclosure = 3
val standardarg = 4
val standardcont = 5
val firstnonstandard = 6

open System.Tags Access

datatype frag
  = FUNC of reg list * cexp option ref
  | STRINGfrag of string

fun standardformals [_,_]   = [standardcont,standardarg]
  | standardformals [_,_,_] = [standardclosure,standardarg,standardcont]
  | standardformals _ = ErrorMsg.impossible "110 in CPSgen"

fun codegen(funs : ((lvar * lvar list * cexp) * bool) list, err) =
let 
    exception Regbind and Labbind
    val labtable : label Intmap.intmap = Intmap.new(32, Labbind)
    val addlabbinding = Intmap.add labtable
    val labmap = Intmap.map labtable

    val nextlabel = ref 0
    fun newlabel() = !nextlabel before inc nextlabel

    val regbindtable : reg Intmap.intmap = Intmap.new(32, Regbind)
    val addbinding = Intmap.add regbindtable
    val regmap = Intmap.map regbindtable

    val nextreg = ref firstnonstandard
    fun newreg _ = !nextreg before inc nextreg

    exception Know
    val knowtable : frag Intmap.intmap = Intmap.new(32, Know)
    val addknow = Intmap.add knowtable
    val know = Intmap.map knowtable

    exception Freemap
    val freemaptable : lvar list Intmap.intmap = Intmap.new(32, Freemap)
    val freemap = Intmap.map freemaptable

    fun makefrag ((f,vl,e),known) = 
	let val lab = newlabel()
	    val fmls = if known then map newreg vl else standardformals vl
	    val knowledge = FUNC(fmls, ref(SOME e))
	 in app2 addbinding (vl,fmls);
            addknow(f, knowledge); addlabbinding(f,lab);
	    FreeMap.freemap (Intmap.add freemaptable) e;
	    (lab,knowledge)
	end

    val frags = ref(map makefrag funs)
    fun addfrag f = frags := f :: !frags

    fun regbind(VAR v) = regmap v
      | regbind(LABEL v) = let val r = newreg()
			   in emit(A.GETLAB{lab=v,dst=r});
			      r
			   end
      | regbind(INT i) = let val r = newreg()
			 in emit(A.ARITHI{oper=iadd,src1=0,src2=i,dst=r});
			     r
		         end
		          ErrorMsg.impossible "Overflow in cps/generic.sml")
      | regbind(STRING s) = let val r = newreg()
				val lab = newlabel()
			     in emit(A.GETLAB{lab=lab,dst=r});
			        addfrag(lab, STRINGfrag s);
				r
			    end
      | regbind(REAL s) = let val r = newreg()
			   in emit(A.GETREAL{value=realc s,dst=r});
			      r
			  end

    fun regbind(VAR v) = regmap v
      | regbind(LABEL v) = let val r = newreg()
			   in emit(A.GETLAB{lab=v,dst=r});
			      r
			   end
      | regbind(INT i) = let val r = newreg()
			 in emit(A.ARITHI{oper=iadd,src1=0,src2=i,dst=r});
			     r
		         end
		          ErrorMsg.impossible "Overflow in cps/generic.sml")
      | regbind(STRING s) = let val r = newreg()
				val lab = newlabel()
			     in emit(A.GETLAB{lab=lab,dst=r});
			        addfrag(lab, STRINGfrag s);
				r
			    end
      | regbind(REAL s) = let val r = newreg()
			   in emit(A.GETREAL{value=realc s,dst=r});
			      r
			  end

    fun regbind(VAR v, f) = f(regmap v)
      | regbind(LABEL v, f) = let val r = newreg()
			       in A.GETLAB{lab=v,dst=r} :: f r
			      end
      | regbind(INT 0, r) = f(zeroreg)
      | regbind(INT i, r) = let val r = newreg()
			     in A.ARITHI{oper=iadd,src1=0,src2=i,dst=r} :: f r
		            end
      | regbind(STRING s, r) = let val r = newreg()
				   val lab = newlabel()
			        in addfrag(lab, STRINGfrag s);
			           A.GETLAB{lab=lab,dst=r} :: f r
			       end
      | regbind(REAL s, r) = let val r = newreg()
			      in A.GETREAL{value=realc s,dst=r} :: f r
			     end

    fun genfrag (lab, FUNC(fmls, ref NONE)) = ()
      | genfrag (lab, FUNC(fmls, ref e)) =
		 (emit(A.LABEL{lab=lab, live=reserved@fmls});
		  gen(e,0))
      | genfrag (lab, STRINGfrag s) =
	(emit(A.WORD{value= size s * power_tags + tag_embedded});
         emit(A.LABEL{lab=lab, live=nil});
	 emit(A.STRING{value= substring(s^"\000\000\000",0,4*((length s+3)div 4))}))

  (* generate a new code label *)
    and genlab(lab, cexp, alloc) = 
	(emit(A.LABEL{lab=lab, live= regmap freevars cexp});
	 gen(cexp,alloc))

    and call(args,fmls,alloc,jmp) =
	let val r = newreg()
	    fun f a = if member a fmls 
			  then let val z=newreg()
			       in emit(A.ARITHI{iadd,src1=0,src2=a,dst=z});
				  z
			       end
			  else a
	 in if alloc<>0 then (emit(A.ARITHI{iadd,src1=allocptr,src2=alloc*4,
					    dst=r});
			      emit(A.MOVE{src=r,dst=allocptr}))
	                else ();
	    app2 (fn(a,b)=>emit(A.MOVE{src=a,dst=b})) 
	         (map (f o regbind) args, fmls);
	    emit jmp
        end

     and binop(oper, (_, [INT k, w],[x],[e]), alloc) =
	      getreg(x, fn x' =>
		     (emit(ARITHI{oper=oper,src1=regbind w,src2=k,dst=x'});
		      gen(e,alloc)))
       | binop(oper, (p, [w, v as INT _],x,e), alloc) =
	      binop(oper, (p, [v,w],x,e), alloc)
       | binop(oper, (_, [v,w],[x],[e]) =>
	      getreg(x, fn x' =>
		  (emit(ARITH{oper=oper,src1=regbind v,src2=regbind w,dst=x'});
		   gen(e,alloc)))

    and gen(cexp,alloc) =
	case cexp
	 of RECORD(_, vl,w,e) =>
	     let fun f(i,r,OFFp 0) = 
		       emit(A.STORE{offset=i*4, src=r, ptr=allocptr})
	           | f(i,r,OFFp k) = 
		       let val r' = newreg()
		       in emit(A.ARITHI{oper=iadd,src1=r,src2=4*k,dst=r'});
			  f(i,r',OFFp 0)
		       end
                   | f(i,r,SELp(k,p)) =
		       let val r' = newreg()
		       in emit(A.FETCH{immutable=true,offset=4*k,
				       ptr=r,dst=r'});
			  f(i,r',p)
		       end
                 fun g(i,nil) = ()
	           | g(i,(z,p)::rest) = (f(i,regbind z,p);
					g(i+1,rest))

             in g(vl);
		getreg(w, fn w' => 
		    emit(A.ARITHI{oper=iadd,src1=allocptr,src2=4*alloc,dst=w}))
		gen(e,alloc+length vl)
	    end
	  | SELECT(i,v,w,e) =>
	    getreg(w, fn w' => (emit(A.FETCH{immutable=true,offset=4*i,
					     ptr=regbind v,dst=w'});
				gen(e,alloc)))
	  | OFFSET(i,v,w,e) =>
	      getreg(w, fn w' => (emit(A.ARITHI{oper=iadd,src1=regbind v,
						src2=4*i,dst=w'});
				  gen(e,alloc)))
	  | APP(func as VAR f, args) => 
		  call(args,standardformals args, alloc,A.JUMP{dst=regbind f})
	  | APP(func as LABEL f, args) =>
	    (case know f
	      of FUNC(fmls,_) => 
		  call(args,fmls,alloc,A.BRANCH{ieq,src1=0,src2=0,
						dst=labmap f}))
	  | APP _ => ErrorMsg.impossible "constant func in CPSgen"
	  | SWITCH(v,l) => 
		let val lab = newlabel()
		    val labs = map (fn _ => newlabel()) l;
		    fun f s = emit(A.LABWORD{lab=s})
		    fun h(lab, e) = genlab(lab, e, alloc)
		    val r = newreg() and r' = newreg() 
		    and r'' = newreg() and r''' = newreg()
		in emit(A.ARITHI{oper=lshift,src1=regbind v,src2=2,dst=r});
		   emit(A.GETLAB{lab=lab,dst=r'});
		   emit(A.ARITH{oper=iadd, src1=r,src2=r',dst=r''});
		   emit(A.FETCH{immutable=true, offset=0, ptr=r'',dst=r'''});
		   emit(A.JUMP{dst=r'''})
		   emit(A.LABEL{lab,live=nil})
		   app f labs;
		   app2 h (labs,l)
		end
        | PRIMOP(P.notb, [v],[x],[e]) =>
	      getreg(x, fn x' =>
		     (emit(ARITHI{oper=xor,src1=regbind v,src2= ~1,dst=x'});
		      gen(e,alloc)))
        | PRIMOP(args as (P.+, _, _, _)) => binop(A.iadd, args, alloc)
        | PRIMOP(args as (P.-, _, _, _)) => binop(A.isub, args, alloc)
        | PRIMOP(args as (P.*, _, _, _)) => binop(A.imul, args, alloc)
        | PRIMOP(args as (P.div, _, _, _)) => binop(A.idiv, args, alloc)
        | PRIMOP(args as (P.fadd, _, _, _)) => binop(A.fadd, args, alloc)
        | PRIMOP(args as (P.fsub, _, _, _)) => binop(A.fsub, args, alloc)
        | PRIMOP(args as (P.fmul, _, _, _)) => binop(A.fmul, args, alloc)
        | PRIMOP(args as (P.fdiv, _, _, _)) => binop(A.fdiv, args, alloc)
        | PRIMOP(args as (P.orb, _, _, _)) => binop(A.orb, args, alloc)
        | PRIMOP(args as (P.andb, _, _, _)) => binop(A.andb, args, alloc)
        | PRIMOP(args as (P.xorb, _, _, _)) => binop(A.xorb, args, alloc)
        | PRIMOP(args as (P.lshift, _, _, _)) => binop(A.rshift, args, alloc)
        | PRIMOP(args as (P.rshift, _, _, _)) => binop(A.rshift, args, alloc)
        | PRIMOP(P.!, [v],[w],[e]) => 
	              gen(PRIMOP(P.subscript,[v, INT 0], [w], [e]))
        | PRIMOP(P.:=, [v,w],[],[e]) => 
	             gen(PRIMOP(P.update, [v,INT 0,w],[],[e]))
        | PRIMOP(P.unboxedassign, [v,w],[],[e]) =>
	      gen(PRIMOP(P.:=, [v,w],[],[e]))
       | PRIMOP(P.~, [v],[w],[e]) =>
	     gen(PRIMOP(P.-,[INT 0,v],[w],[e]))
       | PRIMOP(P.makeref, [v],[w],[e]) =>
	     gen(RECORD(RK_RECORD, [(v,OFFp 0)],[w],[e]))
       | PRIMOP(P.subscript, [v,INT k],[x],[e]) =>
		getreg(x,any, fn x' =>
		   (emit(A.FETCH{immutable=false, offset=4*k,
				 ptr=regbind v,dst=x'});
		    gen(e,alloc)))
       | PRIMOP(P.subscript, [v,w],[x],[e]) =>
		getreg(x,any, fn x' =>
		   let val r = newreg() and r' = newreg()
		   in emit(A.ARITHI{oper=lshift,src1=regbind w,src2=2,dst=r'});
		      emit(A.ARITH{oper=iadd,src1=regbind v, src2=r',dst=r''});
		      emit(A.FETCH{immutable=false, offset=0,ptr=r'',dst=x'});
		      gen(e,alloc)
		   end)
       | PRIMOP(P.update, [a, INT k, v], [], [e]) =>
		   (emit(A.STORE{offset=4*k,ptr=regbind a,src=regbind v});
		    gen(e,alloc))
       | PRIMOP(P.update, [a, i, v], [], [e]) =>
		   let val r = newreg() and r' = newreg()
		   in emit(A.ARITHI{oper=lshift,src1=regbind i,src2=2,dst=r'});
		      emit(A.ARITH{oper=iadd,src1=regbind a, src2=r',dst=r''});
		      emit(A.STORE{offset=0,ptr=r'',src=regbind v});
		      gen(e,alloc)
		   end
       | PRIMOP(P.unboxedupdate, [a, i, v], [], [e]) =>
	     gen(PRIMOP(P.update,[a, i, v], [], [e]))
       | PRIMOP(P.alength, [a], [x], [e]) =>
	     gen(SELECT(~1,a,x,e))
       | PRIMOP(P.slength, [a], [x], [e]) =>
	     gen(SELECT(~1,a,x,e))
       | PRIMOP(P.store, [s,INT k,v], [], [e]) =>
	     (emit(A.STOREB{offset=k,ptr=regbind s, src=regbind v});
	      gen e)
       | PRIMOP(P.store, [s,i,v], [], [e]) =>
	     let val r = newreg()
	     in emit(A.ARITH{oper=iadd,src1=regbind s,src2=regbind i,dst=r});
		emit(A.STOREB{offset=0,ptr=r,src=regbind v});
		gen e
	     end
       | PRIMOP(P.ordof, [s as VAR _, INT k], [v], [e]) =>
	     getreg(v,any, fn v' =>
		   (fetchindexb(regbind s, v', immed k);
		    addl3(v',v',v');
		    addl3(immed 1, v',v');
		    gen e))
       | PRIMOP(P.ordof, [s, INT k], [v], [e]) =>
		getreg(x,any, fn x' =>
		      (emit(A.FETCHB{offset=k,ptr=regbind s,dst=x'});
		       gen(e,alloc)))
       | PRIMOP(P.ordof, [s, i], [x], [e]) =>
		getreg(x,any, fn x' =>
		   let val r = newreg()
		   in emit(A.ARITH{oper=iadd,src1=regbind s,
				   src2=regbind i,dst=r});
		      emit(A.FETCHB{offset=0,ptr=r,dst=x'});
		      gen(e,alloc)
		   end)
       | PRIMOP(P.boxed, [x],[],[a,b]) =>
		    let val lab = newlabel()
		     in emit(A.BRANCH{test=inrange,src1=x,
				      src2=regbind(INT 256),dst=lab});
			gen(a,alloc);
			genlab(lab,b,alloc)
		    end
       | PRIMOP(P.gethdlr, [],[x],[e]) =>
	     getreg(x, fn x' => 
		    (emit(A.ARITHI{oper=iadd,src1=exnhandler,src2=0,dst=x'});
		     gen(e,alloc)))
       | PRIMOP(P.sethdlr, [x],[],[e]) => (move(regbind x, exnptr); gen e)
	     (emit(A.MOVE{src=regbind x, dst=exnhandler});
	      gen e)
       | PRIMOP(P.lessu, [v,w],[],[d,e]) =>
	   let val false_lab = newlabel()
	    in rangeChk(regbind v, regbind w, false_lab); 
	       gen d;
	       genlab(false_lab, e)
	   end
       | PRIMOP(P.gequ, [v,w],[],[d,e]) => (* fill this in sometime *)
       | PRIMOP(args as (P.rangechk,_,_,_)) => compare(A.outofrange,args)
       | PRIMOP(args as (P.ieql,_,_,_)) => compare(A.ine,args)
       | PRIMOP(args as (P.ineq,_,_,_)) => compare(A.ieq,args)
       | PRIMOP(args as (P.>   ,_,_,_)) => compare(A.ile,args)
       | PRIMOP(args as (P.>=  ,_,_,_)) => compare(A.ilt,args)
       | PRIMOP(args as (P.<   ,_,_,_)) => compare(A.ige,args)
       | PRIMOP(args as (P.<=  ,_,_,_)) => compare(A.igt,args)
       | PRIMOP(args as (P.feql,_,_,_)) => compare(A.fne,args)
       | PRIMOP(args as (P.fneq,_,_,_)) => compare(A.feq,args)
       | PRIMOP(args as (P.fgt ,_,_,_)) => compare(A.fle,args)
       | PRIMOP(args as (P.flt ,_,_,_)) => compare(A.fge,args)
       | PRIMOP(args as (P.fge ,_,_,_)) => compare(A.flt,args)
       | PRIMOP(args as (P.fle ,_,_,_)) => compare(A.fgt,args)
       | _ => ErrorMsg.impossible "3312 in CPSgen"

    and compare(test, (_,[v,w],[],[d,e])) =
		let val lab = newlabel()
		 in emit(A.BRANCH{test=test,src1=regbind v, 
				  src2=regbind w, dst=lab}); 
		    gen d; genlab(lab, e, alloc)
		end
       
in  (* not necessary with regmasks: emitlong 1; Bogus tag for spacing, boot_v. *)
    let fun loop nil = ()
          | loop (frag::r) = (frags := r; genfrag frag; loop(!frags))
    in loop(!frags)
    end
end

end
