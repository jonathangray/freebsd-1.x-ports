(* Copyright 1989 by AT&T Bell Laboratories *)
functor Contract(val maxfree : int) :
	sig val contract : {function: CPS.function,
			    click: string->unit,
			    last: bool,
			    arg: System.Unsafe.object option}
					-> CPS.function
        end =
struct

 open Access CPS
 structure CG = System.Control.CG

 fun map1 f (a,b) = (f a, b)

 fun last0[x]=x | last0(a::b)=last0 b | last0 _ = 0
			  
 fun sameName(x,VAR y) = Access.sameName(x,y) 
   | sameName(x,LABEL y) = Access.sameName(x,y) 
   | sameName _ = ()

 val equalUptoAlpha =
 let fun equ pairs =
        let fun same(VAR a, VAR b) = 
	          let fun look((x,y)::rest) = a=x andalso b=y orelse look rest
		        | look nil = false
		   in a=b orelse look pairs
		  end
              | same(LABEL a, LABEL b) = same(VAR a, VAR b)
              | same(INT i, INT j) = i=j
              | same(REAL a, REAL b) = a=b
              | same(STRING a, STRING b) = a=b
	      | same(a,b) = false
            fun samefields((a,ap)::ar,(b,bp)::br) = ap=bp andalso same(a,b)
		                                     andalso samefields(ar,br)
              | samefields(nil,nil) = true
              | samefields _ = false
	    fun samewith p = equ (p::pairs)
            fun all2 f (e::r,e'::r') = f(e,e') andalso all2 f (r,r')
              | all2 f (nil,nil) = true
              | all2 f _ = false
            val rec sameexp = 
	     fn (SELECT(i,v,w,e),SELECT(i',v',w',e')) =>
		     i=i' andalso same(v,v') andalso samewith(w,w') (e,e')
              | (RECORD(k,vl,w,e),RECORD(k',vl',w',e')) =>
		     (k = k') andalso samefields(vl,vl') andalso samewith (w,w') (e,e')
              | (OFFSET(i,v,w,e),OFFSET(i',v',w',e')) =>
		     i=i' andalso same(v,v') andalso samewith(w,w') (e,e')
              | (SWITCH(v,c,el),SWITCH(v',c',el')) =>
		    same(v,v') andalso all2 (samewith(c,c')) (el,el')
	      | (APP(f,vl),APP(f',vl')) => same(f,f') andalso all2 same (vl,vl')
              | (FIX(l,e),FIX(l',e')) => (* punt! *) false
	      | (BRANCH(i,vl,c,e1,e2),BRANCH(i',vl',c',e1',e2')) =>
		    i=i' andalso all2 same (vl,vl') 
		    andalso samewith(c,c') (e1,e1')
		    andalso samewith(c,c') (e2,e2')
	      | (LOOKER(i,vl,w,e),LOOKER(i',vl',w',e')) =>
		   i=i' andalso all2 same (vl,vl') andalso samewith(w,w')(e,e')
	      | (SETTER(i,vl,e),SETTER(i',vl',e')) =>
		   i=i' andalso all2 same (vl,vl') andalso sameexp(e,e')
	      | (ARITH(i,vl,w,e),ARITH(i',vl',w',e')) =>
		   i=i' andalso all2 same (vl,vl') andalso samewith(w,w')(e,e')
	      | (PURE(i,vl,w,e),PURE(i',vl',w',e')) =>
		   i=i' andalso all2 same (vl,vl') andalso samewith(w,w')(e,e')
	      | _ => false
         in sameexp
        end
   in equ nil
  end

 datatype arity = BOT 
		| UNK  (* an arg seen that isn't a known record *)
		| COUNT of int * bool (* int is # of record fields;
		       bool is whether any arguments were unknown records*)
		| TOP

 datatype info = FNinfo of {arity: arity list ref,
			    args: lvar list,
			    body : cexp option ref,
			    specialuse: int ref option ref}
	       | RECinfo of (value * accesspath) list
	       | SELinfo of int * value
	       | ARGinfo of {biggestSEL: int ref}
	       | OFFinfo of int * value
	       | MISCinfo

fun contract {function=(fvar,fargs,cexp), click, last, arg} =
let
 val debug = !System.Control.CG.misc1 (* false *)
 fun debugprint s = if debug then System.Print.say(s) else ()
 fun debugflush() = if debug then System.Print.flush() else ()

 (* Note that maxfree has already been reduced by 1 (in CPScomp)
    on most machines to allow for an arithtemp *)
 val maxregs = min(!CG.maxregs, maxfree)- !CG.calleesaves

     val say = debugprint
     fun sayv(VAR v) = say(Access.lvarName v)
        | sayv(LABEL v) = say("(L)" ^ Access.lvarName v)
	| sayv(INT i) = say(makestring i)
	| sayv(REAL r) = say r
	| sayv(STRING s) = (say "\""; say s; say "\"")

 val botlist = if !CG.flattenargs then map (fn _ => BOT)
				  else map (fn _ => TOP)
 exception Escapemap
 val m : {info: info, used : int ref, escape : int ref} Intmap.intmap =
		 Intmap.new(128, Escapemap)
 val get = Intmap.map m
 val get = fn i => get i handle Escapemap => ErrorMsg.impossible
				      ("Escapemap on " ^ makestring i)
 val enter = Intmap.add m
 fun use(VAR v) = inc(#used(get v))
   | use(LABEL v) = inc(#used(get v))
   | use _ = ()
 fun used v = !(#used(get v)) > 0

 fun escape(VAR v) = let val {escape,used,...} = get v
		      in inc escape; inc used
		     end
   | escape(LABEL v) = escape(VAR v)
   | escape _ = ()

 fun selectFrom(VAR v,i) =
       let val {info,used,escape,...} = get v
       in inc used; if !CG.selectopt then () else inc escape;
	  case info
	   of ARGinfo{biggestSEL as ref j} => biggestSEL := max(i,j)
	    | _ => ()
       end
   | selectFrom(LABEL v, i) = selectFrom(VAR v, i)
   | selectFrom _ = ()

 fun enterField(v, SELp(i,_)) = selectFrom(v,i)
   | enterField(v, _) = escape(v)

 fun enterREC(w,vl) = enter(w,{info=RECinfo vl, escape=ref 0,used=ref 0})
 fun enterARG w = enter(w,{info=ARGinfo{biggestSEL=ref ~1},
			   escape=ref 0, used = ref 0})
 fun enterMISC w = enter(w,{info=MISCinfo, escape=ref 0, used = ref 0})

 fun enterFN (f,vl,cexp) =
	    (enter(f,{escape=ref 0,used=ref 0,
		     info=FNinfo{arity=ref(botlist vl), args=vl, 
		     body= ref(if !CG.betacontract then SOME cexp else NONE),
		     specialuse=ref NONE}});
	     app enterARG vl)

 local exception Found
 in fun findFetch(v,k) body =
      (* find whether field k of variable v is guaranteed to exist *)
      let fun f(RECORD(_, fields,_,e)) = (app g fields; f e)
	 | f(SELECT(i,VAR v',w,e)) = if v=v' andalso i=k then raise Found
					else f e
	 | f(SELECT(_,_,_,e)) = f e
	 | f(OFFSET(_,_,_,e)) = f e
	 | f(FIX(_,e)) = f e
	 | f(BRANCH(_,_,_,e1,e2)) = findFetch(v,k) e1 andalso
				    findFetch(v,k) e2
	 | f(LOOKER(_,_,_,e)) = f e
	 | f(SETTER(_,_,e)) = f e
	 | f(ARITH(_,_,_,e)) = f e
	 | f(PURE(_,_,_,e)) = f e
	 | f(SWITCH(_,_,el)) = not(exists (not o findFetch(v,k)) el)
	 | f _ = false
       and g(VAR v',SELp(i,_)) = 
		      if v=v' andalso i=k then raise Found else ()
	 | g _  = ()
      in f body 
	 handle Found => true
      end
 end

 fun don'tReduce(VAR g) = (case get g
			    of {info=FNinfo{body,...},...} => body := NONE
			     | _ => ())
   | don'tReduce(LABEL g) = don'tReduce(VAR g)
   | don'tReduce _ = ()

 fun SELECTandAPP(APP(g,_)) = SOME g
   | SELECTandAPP(SELECT(_,_,_,e)) = SELECTandAPP e
   | SELECTandAPP _ = NONE

 fun checkreduce(f,vl,body) =
       (if last then () 
	else case (vl,body)
	 of ([a,c],FIX([(h,[b,k],body)],APP(VAR c',[VAR h']))) =>
		(case (c=c' andalso h=h', SELECTandAPP(body))
		  of (true,SOME g) => don'tReduce g
		   | _ => ())
	  | _ => ();
       case get f of
	 {escape=ref 0,used=ref 2,
	  info=FNinfo{specialuse=ref(SOME(ref 1)),body=b,...},...} =>
	     if not (!CG.ifidiom) then b:=NONE else ()
       | {escape=ref 0,used=ref i,
	info=FNinfo{body=b,arity as ref al,...},...} =>
	      let fun loop(v::vl,a::al, headroom) =
		   (case (get v, a)
		     of ({used=ref 0,...},_) =>
			    if !CG.dropargs 
				 then COUNT(0,true)::loop(vl,al,headroom+1)
				 else a::loop(vl,al,headroom)
		      | ({escape=ref 0,...}, COUNT(c,false)) => 
			      if headroom+1-c >= 0
				 then a::loop(vl,al,headroom+1-c)
				 else TOP::loop(vl,al,headroom)
		      | ({escape=ref 0,info=ARGinfo{biggestSEL=ref j},...},
			 COUNT(c,true)) => 
			     if j=c-1 andalso findFetch(v,c-1) body
				 andalso headroom+1-c >= 0
				 then a::loop(vl,al,headroom+1-c)
				 else TOP::loop(vl,al,headroom)
		      | _ => TOP::loop(vl,al,headroom))
		    | loop _ = []
	      in if i>1 then b := NONE else ();
	    (* We have maxregs registers; one might be used for a closure
	       argument; so the most arguments we can give a function 
	       here is maxregs-1  *)
		 arity := loop(vl,al,maxregs-1-length(al))
	      end
	  | {info=FNinfo{body=b,...},...} =>
		(b := NONE;
		 if last
		 then ()
		 else (case body of
			APP(g, _) => don'tReduce g
		       | _ => ())))

 exception ConstFold

 val rec pass1 = 
  fn RECORD(_, vl,w,e) => (enterREC(w,vl); app enterField vl; pass1 e)
   | SELECT (i,v,w,e) => 
	    (enter(w,{info=SELinfo(i,v), escape=ref 0, used = ref 0});
	     selectFrom(v,i);
	     pass1 e)
   | OFFSET (i,v,w,e) => 
	    (enter(w,{info=OFFinfo(i,v), escape=ref 0, used=ref 0});
	     escape v; pass1 e)
   | APP(LABEL f, vl) => pass1(APP(VAR f, vl))
   | APP(VAR f, vl) =>
     ((case get f
	of {info=FNinfo{arity as ref al,...},...} =>
	 let fun loop(BOT::r,vl0 as VAR v :: vl, n) =
		  (case get v
		    of {info=RECinfo wl,...} =>
			    loop(COUNT(length wl,false)::r,vl0,n)
		     | _ => UNK::loop(r,vl,n+1))
	       | loop(UNK::r,vl0 as VAR v :: vl, n) =
		  (case get v
		    of {info=RECinfo wl,...} =>
			       loop(COUNT(length wl,true)::r,vl0,n)
		     | _ => UNK::loop(r,vl,n+1))
	       | loop((cnt as COUNT(a,unk))::r, VAR v::vl,n) = 
		  (case get v of
		     {info=RECinfo wl, ...} =>
		     if a = length wl
		     then cnt::loop(r,vl,n+1)
		     else TOP::loop(r,vl,n+1)
		   | _ => COUNT(a,true)::loop(r,vl,n+1))
	       | loop(_::r, _::vl,n) = TOP::loop(r,vl,n+1)
	       | loop _ = []
	 in arity := loop(al,vl,0)
	 end
	| _ => ());
      use(VAR f); app escape vl)
   | APP(f, vl) => (use f; app escape vl)
   | FIX(l, e) => (app enterFN l;
		   app (fn (f,vl,body) => pass1 body) l;
		   pass1 e;
		   app checkreduce l)
   | SWITCH(v,_,el) => (use v; app pass1 el)
   | BRANCH(i,vl,c,e1 as APP(VAR f1, [INT 1]),
		   e2 as APP(VAR f2, [INT 0])) =>
	 (case get f1
	   of {info=FNinfo{body=ref(SOME(BRANCH(P.ineq,[INT 0, VAR w2],_,_,_))),
			   args=[w1],specialuse,...},...} =>
	       if f1=f2 andalso w1=w2 
		   then let val {used,...}=get w1
			 in specialuse := SOME used
			end
		   else ()
	    | _ => ();
	  app escape vl; pass1 e1; pass1 e2)
   | BRANCH(P.boxed,[v],_,e1,e2) => (escape v; pass1 e1; pass1 e2)
   | BRANCH(P.unboxed,[v],_,e1,e2) => (escape v; pass1 e1; pass1 e2)
   | BRANCH(i,vl,_,e1,e2) => (app escape vl; pass1 e1; pass1 e2)
	  (* the abovementioned escape is necessary, instead of use,
	    in case record pointers are compared for equality *)
   | SETTER(i,vl,e) => (app escape vl; pass1 e)
   | LOOKER(i,vl,w,e) => (app use vl; enterMISC w; pass1 e)
   | ARITH(i,vl,w,e) => (app use vl; enterMISC w; pass1 e)
   | PURE(i,vl,w,e) => (app escape vl; enterMISC w; pass1 e)

 exception Beta
 val m2 : value Intmap.intmap = Intmap.new(32, Beta)
 local val mapm2 = Intmap.map m2
   in fun ren(v0 as VAR v) = (ren(mapm2 v) handle Beta => v0)
	| ren(v0 as LABEL v) = (ren(mapm2 v) handle Beta => v0)
	| ren x = x
 end
 fun newname vw = (sameName vw; Intmap.add m2 vw)
 fun newnames(v::vl, w::wl) = (newname(v,w); newnames(vl,wl))
   | newnames _ = ()

 fun makeSELECT(i,v,w,e) =
       case ren v
        of v' as VAR v'' =>
		    (case get v''
		      of {info=RECinfo vl,...} =>
				(click "d"; newname(w,#1(nth(vl,i))); e())
		       | _ => SELECT(i,v', w, e()))
	 | v' => SELECT(i,v',w,e())

  fun setter (P.update, [_, _, INT _]) = P.unboxedupdate
    | setter (P.update, [_, _, REAL _]) = P.boxedupdate
    | setter (P.update, [_, _, STRING _]) = P.boxedupdate
    | setter (P.update, [_, _, VAR v]) = (case #info(get v)
	 of (FNinfo _) => P.boxedupdate
	  | (RECinfo _) => P.boxedupdate
	  | (OFFinfo _) => P.boxedupdate
	  | _ => P.update
	(* end case *))
    | setter (i, _) = i

 val rec reduce = fn cexp => g NONE cexp
 and g = fn hdlr =>
 let val rec g' =
   fn RECORD (k,vl,w,e) =>
      let val {info=RECinfo _,used=ref use,...} = get w
      in if use=0 andalso !CG.deadvars
	 then (click "b"; g' e)
	 else RECORD(k,map (map1 ren) vl, w, g' e)
      end
    | SELECT(i,v,w,e) =>
	  if not(used w) andalso !CG.deadvars then (click "c"; g' e)
	  else makeSELECT(i,v,w, fn () => g' e)
    | OFFSET(i,v,w,e) => OFFSET(i,ren v,w,g' e)
    | APP(f, vl) =>
      let fun trybeta(f',fv) = 
	  (case (get fv, vl)
	   of ({info=FNinfo{args,body as ref(SOME b),...},...},_) =>
		    (newnames(args, map ren vl); body := NONE; g' b)
	    | ({info=FNinfo{arity=ref al,...},escape=ref 0,...}, _) =>
	      let fun loop(COUNT(cnt,_) :: r, v::vl,args) =
		      let fun g(i,args) = 
			      if i=cnt then loop(r,vl,args)
				  else let val z = mkLvar()
					in enter(z,{info=SELinfo(i,v),
						    escape=ref 3,used=ref 3});
					   makeSELECT(i,v,z,
					       fn ()=>g(i+1, ren(VAR z) :: args))
				       end
		       in g(0,args)
		      end
		    | loop(_::r,v::vl,args) = loop(r,vl, ren v :: args)
		    | loop (_,_,args) = APP(f', rev args)
	      in loop(al,vl,nil)
	      end
	    | (_,vl') => APP(f', map ren vl'))
      in case ren f
	of f' as VAR fv => trybeta(f',fv)
	 | f' as LABEL fv => trybeta(f',fv)
	 | f' => APP(f', map ren vl)
      end
    | FIX(l,e) =>
      let fun process_args(f,vl,body) = 
	       case get f
		of {info=FNinfo{body=ref NONE,arity=ref al,...},
				escape=ref 0,...} =>
		      let fun vars 0 = []
			    | vars i = mkLvar()::vars(i-1)
			  fun newargs(COUNT(j,_) :: r,v::vl) =
			      let val new = vars j
			      in app enterMISC new;
				 enterREC(v, map (fn x =>(VAR x, OFFp 0)) new);
				 click "f";
				 new @ newargs(r,vl)
			      end
			    | newargs(_::r,v::vl) = v::newargs(r,vl)
			    | newargs _ = []
		      in (f, newargs(al,vl), body)
		      end
		 | _ => (f, vl, body)

	  fun drop_dead ((f,vl,body as BRANCH(_,[_,VAR w],_,_,_))::rest) = 
	      (case (get f, get w)
		of ({info=FNinfo{body = b as ref(SOME _),...}, used=ref 2,...},
		    {used=ref uw,...})=> 
			(if uw<>1 then (b:=NONE; 
					(f,vl,body)::drop_dead rest)
				  else (click "E";
				 	drop_dead rest))
		 | ({info=FNinfo{body=ref(SOME _),...}, used=ref 1,...},
		    _)=> 
			(click "e";
			 drop_dead rest)
		 | ({info=FNinfo{body as ref(SOME _),...}, used=ref 0,...},
		    _)=> 
			(click "g";
			 body:=NONE;
			 drop_dead rest)
		 | _ => (f,vl, body) :: drop_dead rest)
	    | drop_dead ((f,vl,body)::rest) = 
	      (case get f
		of {info=FNinfo{body=ref(SOME _),...}, used=ref 1,...}=>
			(click "e";
			 drop_dead rest)
		 | {info=FNinfo{body as ref(SOME _),...}, used=ref 0,...}=>
			(click "g";
			 body := NONE;
			 drop_dead rest)
		 | _ => (f,vl,body) :: drop_dead rest)
	    | drop_dead nil = nil
	  fun reduce_body (f,vl,body) = (f,vl,reduce body)
       in case  map reduce_body (drop_dead (map process_args l))
	   of nil => g' e
	    | l' => FIX(l', g' e)
      end
    | SWITCH(v,c,el) => 
	    (case ren v
	      of v' as INT i => 
		    if !CG.switchopt 
			    then (click "h"; 
				  newname(c,INT 0);
				  g' (nth(el,i)))
			    else SWITCH(v', c, map g' el)
	       | v' => SWITCH(v',c, map g' el))
    | LOOKER(P.gethdlr,_,w,e) =>
      (if !CG.handlerfold
	then case hdlr of
	 NONE => if used w then LOOKER(P.gethdlr,[],w,g (SOME(VAR w)) e)
		 else (click "i"; g' e)
       | SOME w' => (click "j"; newname(w,w'); g' e)
	else LOOKER(P.gethdlr,[],w,g (SOME(VAR w)) e))
    | SETTER(P.sethdlr,[v as VAR vv],e) =>
      let val v' as VAR vv' = ren v
	  val e' = g (SOME v') e
      in if !CG.handlerfold
	then case hdlr of
	   NONE => SETTER(P.sethdlr,[v'],e')
	 | SOME (v'' as VAR vv'') => if vv'=vv'' then (click "k"; e')
		       else SETTER(P.sethdlr,[v'],e')
	else SETTER(P.sethdlr,[v'],e')
      end
(**    | SETTER(i,vl,e) => SETTER(i, map ren vl, g' e)**)
    | SETTER(i, vl, e) => let
	val vl' = map ren vl
	in
	  SETTER(setter (i, vl'), vl', g' e)
	end
    | LOOKER(i,vl,w,e) => 
	  if not(used w) andalso !CG.deadvars
	      then (click "m"; g' e)
	      else LOOKER(i, map ren vl, w, g' e)
    | ARITH(i,vl,w,e) =>
	     let val vl' = map ren vl
	      in  (if !CG.arithopt
		     then (newname(w,arith(i, vl')); g' e)
		     else raise ConstFold)
		 handle ConstFold => ARITH(i, vl', w, g' e)
		      | Overflow => ARITH(i, vl', w, g' e)
	     end
    | PURE(i,vl,w,e) =>
      if not(used w) andalso !CG.deadvars
	then (click "m"; g' e)
	else let val vl' = map ren vl
	      in  (if !CG.arithopt
		     then (newname(w,pure(i, vl')); g' e)
		     else raise ConstFold)
		 handle ConstFold => PURE(i, vl', w, g' e)
	     end
    | BRANCH(i,vl,c,e1,e2) =>
       let val vl' = map ren vl
	   fun h() = (if !CG.branchfold andalso equalUptoAlpha(e1,e2)
		     then (click "z"; newname(c,INT 0); g' e1)
		     else if !CG.comparefold
		     then if branch(i,vl')
			   then (newname(c,INT 0); g' e1)
			   else (newname(c,INT 0); g' e2)
		     else raise ConstFold)
		 handle ConstFold => BRANCH(i, vl', c, g' e1, g' e2)
        in case (e1,e2) 
            of (APP(VAR f, [INT 1]), APP(VAR f', [INT 0])) =>
		(case (f=f', get f)
                  of (true, 
	              {info=FNinfo{args,body as ref(SOME(BRANCH(_,_,c',a,b))),
		       		   ...},...}) =>
                         (newname(c', VAR c); 
		          body:=NONE;
			  g'(BRANCH(i,vl,c,a,b)))
	           | _ => h())
	     | _ => h()
       end
  in g'
 end

 and branch =
    fn (P.unboxed, vl) => not(branch(P.boxed, vl))
     | (P.boxed, [INT _]) => (click "n"; false)
     | (P.boxed, [STRING s]) => (click "o"; true)
     | (P.boxed, [VAR v]) => 
	 (case get v
	   of  {info=RECinfo _, ...} => (click "p"; true)
	    | _ => raise ConstFold)
     | (P.<, [VAR v, VAR w]) => 
	  if v=w then (click "v"; false) else raise ConstFold
     | (P.<, [INT i, INT j]) => (click "w"; i<j)
     | (P.>, [w,v]) => branch(P.<,[v,w])
     | (P.<=, [w,v]) => branch(P.>=,[v,w])
     | (P.>=, vl) => not(branch(P.<,vl))
     | (P.gequ, [v,w]) => not(branch(P.lessu, [v,w]))
     | (P.lessu, [VAR v, VAR w]) => if v=w then (click "v"; false) 
					   else raise ConstFold
     | (P.lessu, [INT i, INT j]) => 
		    (click "w"; 
		     if j<0 then i>=0 orelse i<j
			   else i>=0 andalso i<j)
     | (P.ieql, [VAR v, VAR w]) => (if v=w then (click "v"; true)
					else raise ConstFold)
     | (P.ieql, [INT i, INT j]) => (click "w"; i=j)
     | (P.ineq, [v,w]) => not(branch(P.ieql,[w,v]))
     | _ => raise ConstFold

  and arith =
    fn (P.*, [INT 1, v]) => (click "F"; v)
     | (P.*, [v, INT 1]) => (click "G"; v)
     | (P.*, [INT 0, _]) => (click "H"; INT 0)
     | (P.*, [_, INT 0]) => (click "I"; INT 0)
     | (P.*, [INT i, INT j]) =>
		let val x = i*j in x+x; click "J"; INT x end
     | (P.div, [v, INT 1]) => (click "K"; v)
     | (P.div, [INT i, INT 0]) => raise ConstFold
     | (P.div, [INT i, INT j]) =>
		let val x = i quot j in x+x; click "L"; INT x end
     | (P.+, [INT 0, v]) => (click "M"; v)
     | (P.+, [v, INT 0]) => (click "N"; v)
     | (P.+, [INT i, INT j]) =>
	       let val x = i+j in x+x; click "O"; INT x end
     | (P.-, [v, INT 0]) => (click "P"; v)
     | (P.-, [INT i, INT j]) =>
	       let val x = i-j in x+x; click "Q"; INT x end
     | (P.~, [INT i]) =>
		  let val x = ~i in x+x; click "X"; INT x end
     | _ => raise ConstFold

  and pure =
    fn (P.rshift, [INT i, INT j]) => (click "R"; INT(Bits.rshift(i,j)))
     | (P.rshift, [INT 0, _]) => (click "S"; INT 0)
     | (P.rshift, [v, INT 0]) => (click "T"; v)
     | (P.length, [STRING s]) => (click "V"; INT(size s))
(*         | (P.ordof, [STRING s, INT i]) => (click "W"; INT(ordof(s,i))) *)
     | (P.lshift, [INT i, INT j]) =>
		       (let val x = Bits.lshift(i,j)
			in x+x; click "Y"; INT x
			end handle Overflow => raise ConstFold)
     | (P.lshift, [INT 0, _]) => (click "Z"; INT 0)
     | (P.lshift, [v, INT 0]) => (click "1"; v)
     | (P.orb, [INT i, INT j]) => (click "2"; INT(Bits.orb(i,j)))
     | (P.orb, [INT 0, v]) => (click "3"; v)
     | (P.orb, [v, INT 0]) => (click "4"; v)
     | (P.xorb, [INT i, INT j]) => (click "5"; INT(Bits.xorb(i,j)))
     | (P.xorb, [INT 0, v]) => (click "6"; v)
     | (P.xorb, [v, INT 0]) => (click "7"; v)
     | (P.notb, [INT i]) => (click "8"; INT(Bits.notb i))
     | (P.andb, [INT i, INT j]) => (click "9"; INT(Bits.andb(i,j)))
     | (P.andb, [INT 0, _]) => (click "0"; INT 0)
     | (P.andb, [_, INT 0]) => (click "T"; INT 0)
     | (P.real, [INT i]) => (REAL(makestring i ^ ".0"))  (* isn't this cool? *)
     | _ => raise ConstFold

 val _ = (debugprint "\nContract: "; debugflush())
 fun ssss cexp = if debug then (debugprint "\nAfter contract: \n"; 
				if !CG.misc4=16 then
				    CPSprint.show System.Print.say cexp
				else ();
				cexp)
		 else cexp

in enterMISC fvar; app enterMISC fargs;
   pass1 cexp;
   (fvar, fargs, ssss(reduce cexp))
end

end

