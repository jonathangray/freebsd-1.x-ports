
(* Copyright 1989 by AT&T Bell Laboratories *)
functor VaxCM(V : VAXCODER) : CMACHINE = struct

structure V' : sig exception BadReal of string
		   datatype Register = reg of int

		   eqtype Label sharing type Label = V.Label
		   datatype EA = direct of Register
			    | autoinc of Register
			    | autodec of Register
			    | displace of int * Register
			    | deferred of int * Register
			    | immed of int
			    | immedlab of Label
			    | address of Label
			    | index of EA * Register

		end = V
open V' System.Tags

datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

fun defer(direct r) = displace(0,r)
  | defer(displace z) = deferred z
  | defer(immedlab lab) = address lab
  | defer _ = ErrorMsg.impossible "defer in cpsvax"

val sp' = reg 14
val exnptr = direct(reg 13)
val dataptr as direct dataptr' = direct(reg 12)
val datalimit = direct(reg 8)
val arithtemps as [arithtemp as direct arithtemp'] = [direct(reg 9)]
val storeptr = direct(reg 11)
val standardclosure = direct(reg 2)
val standardarg = direct(reg 0)
val standardcont = direct(reg 1)
val standardlink = direct(reg 3)
val miscregs = map (direct o reg) [4,5,6,7,10]
val savedfpregs = [] : EA list
val N_FLOAT_REGS = 16
val varptr = displace(N_FLOAT_REGS*8,sp')
val varptr_indexable = false
val fp_base = sp'
val floatregs = 
    let fun from(n,m) = if n>=m then [] else n::from(n+1,m)
    in map (fn r => displace(r*8, fp_base)) (from(0,N_FLOAT_REGS))
    end

fun newlabel() = immedlab(V.newlabel())
fun emitlab(i,immedlab lab) = V.emitlab(i,lab)
fun define (immedlab lab) = V.define lab

fun beginStdFn _ = ()

(* checkLimit (n, lab):
 * Generate code to check the heap limit to see if there is enough free space
 * to allocate n bytes.
 *)
fun testLimit()=
    V.cmpl(dataptr,datalimit)

val startgc_offset = 0
val mask_offset = 4 (* not really 4, fix this some time *)

fun checkLimit (max_allocation,lab,mask) = 
   let val lab' = V.newlabel()
    in V.comment ("begin fun, max alloc = "^(makestring max_allocation)^"\n");
       if max_allocation >= 4096
	   then (V.addl3(dataptr,immed(max_allocation-4096),arithtemp);
		 V.cmpl(arithtemp,datalimit))
	   else ();
       V.bleq(address lab');
       V.movl(mask, displace(mask_offset,sp'));
       V.movl(lab, arithtemp);
       V.jmp(deferred(startgc_offset,sp'));
       V.define lab'
   end

val align = V.align
val mark = V.mark
fun move(args2 as (displace(_, b1),displace(_, b2))) =
    if b1 = fp_base andalso b2 = fp_base then V.movg args2 else V.movl args2
  | move args2 = V.movl args2
val emitlong = V.emitlong
val realconst = V.realconst
val emitstring = V.emitstring

fun jmpindexb(lab,direct y) = V.jmp(index(defer lab, y))
  | jmpindexb(lab,y) = (move(y,arithtemp); V.jmp(index(defer lab, arithtemp')))

fun record(vl, z) =
    let open CPS
	val len = List.length vl
	fun f(i,nil) = ()
	  | f(i,(direct r, SELp(j,p))::rest) = f(i,(displace(j*4,r),p)::rest)
	  | f(i,z::(direct s, SELp(j,p))::rest) =
			f(i,z::(displace(4*j,s),p)::rest)
	  | f(i,(x as direct(reg r), OFFp 0)::
		(y0 as (y as direct(reg s), OFFp 0))::rest) =
		if (s+1=r) then (V.movq(y,displace((i-1)*4,dataptr'));
				 f(i-2,rest))
		    else (V.movl(x,displace(i*4,dataptr')); f(i-1,y0::rest))
	  | f(i,(x as displace(j,r),OFFp 0)::(y as displace(k,s),OFFp 0)::rest) =
		if k+4=j andalso r=s
		     then (V.movq(y,displace((i-1)*4,dataptr')); f(i-2,rest))
		     else (V.movl(x,displace(i*4,dataptr')); 
			   f(i-1,(y,OFFp 0)::rest))
	  | f(i,(x,OFFp 0)::rest) = (V.movl(x,displace(i*4,dataptr'));
				     f(i-1,rest))
	  | f(i,(displace kr,SELp(0,p))::rest) = f(i,(deferred kr,p)::rest)
	  | f(i,(direct r, OFFp j)::rest) = (V.moval(displace(j*4,r),
						    displace(i*4,dataptr'));
						 f(i-1,rest))
	  | f(i,(x,p)::rest) = (V.movl(x,arithtemp); f(i,(arithtemp,p)::rest))
      in f(len - 2, rev vl);
	 V.movl(dataptr,z);
         V.addl2(immed(4*len), dataptr)
     end

  (* recordStore(x, y, alwaysBoxed) records a store operation into mem[x+2*(z-1)].
   * The flag alwaysBoxed is true if the value stored is guaranteed to be boxed.
   *)
    fun recordStore (x, y, _) = record ([
	    (immed(System.Tags.make_desc(3, System.Tags.tag_record)), CPS.OFFp 0),
	    (x, CPS.OFFp 0), (y, CPS.OFFp 0), (storeptr, CPS.OFFp 0)
	  ], storeptr)

fun select(i, direct r, s) = V.movl(displace(i*4,r),s)
  | select(0, a, s) = V.movl(defer a, s)

fun offset(i, direct r, s) = V.moval(displace(i*4,r),s)

val add = V.addl3
val addt = add
val op sub = V.subl3
val subt = op sub
val ashl = V.ashl
fun ashr(immed i, b, c) = V.ashl(immed (~i), b, c)
  | ashr(a,b,c) = (V.subl3(a,immed 0,c);
		   V.ashl(c,b,c))

val mult = V.mull2
val divt = V.divl2
val orb = V.bisl3
fun andb (a,b,c) = (V.subl3(a,immed ~1,arithtemp);  (* potential bug, if
					      generic.sml is changed! *)
		    V.bicl3(arithtemp,b,c))
fun notb (a,b) = V.subl3(a,immed(~1),b)
val xorb = V.xorl3

					(* y <- mem[x+4*(z-1)] *)
fun fetchindexd(x as direct x',y as displace(_,base),z) = 
    if base <> fp_base then 
	ErrorMsg.impossible "vax/vax/fetchindexd: dst not floating register"
    else (case z 
	    of immed i => V.movg(displace(4*(i-1),x'), y)
	     | direct _ => (V.ashl(immed 2,z,arithtemp);
			    V.addl3(arithtemp,x,arithtemp);
			    V.movg(displace(~4,arithtemp'),y))
	     | _ => ErrorMsg.impossible "vax/vax/fetchindexd: bad index")
  | fetchindexd _ =  ErrorMsg.impossible "vax/vax/fetchindexd"

					(* mem[y+4*(z-1)] <- x *)
fun storeindexd(x as displace(_,base),y as direct y',z) =
    if base <> fp_base then
	ErrorMsg.impossible "vax/vax/storeindexd: src not floating register"
    else (case z 
	    of immed i => V.movg(x,displace(4*(i-1),y'))
	     | direct _ => (V.ashl(immed 2,z,arithtemp);
			    V.addl3(arithtemp,y,arithtemp);
			    V.movg(x,displace(~4,arithtemp')))
	     | _ => ErrorMsg.impossible "vax/vax/storeindexd: bad index")
  | storeindexd _ = ErrorMsg.impossible "vax/vax/storeindexd"

fun fetchindexl(v,w, immed 1) = V.movl(defer v, w)
  | fetchindexl(direct v, w, immed k) = V.movl(displace(2*k-2,v), w)
  | fetchindexl(v,w,i) = 
	    (V.ashl(immed ~1,i,arithtemp);
	     V.movl(index(defer v, arithtemp'),w))
fun storeindexl(v,w, immed 1) = V.movl(v, defer w)
  | storeindexl(v, direct w, immed k) = V.movl(v, displace(2*k-2, w))
  | storeindexl(v,w,i) = 
	    (V.ashl(immed ~1,i,arithtemp);
	     V.movl(v,index(defer w, arithtemp')))

(* fetchindexb(x,y,z) fetches a byte: y <- mem[x+z], where y is not arithtemp *)
fun fetchindexb (x, y, direct indx) = V.movzbl(index(defer x, indx), y)
  | fetchindexb (direct x, y, immed indx) = V.movzbl(displace(indx, x), y)

(* storeindexb(x,y,z) stores a byte: mem[y+z] <- x. *)
fun storeindexb (x, y, direct indx) = V.movb(x, index(defer y, indx))
  | storeindexb (x, direct y, immed indx) = V.movb(x, displace(indx, y))

fun loadfloat(src,dst) =
    let val msg = "vax/vax/loadfloat: Bad destination register"
    in case dst 
	 of displace(_,base) =>  if base = fp_base then V.movg(defer src, dst)
				  else ErrorMsg.impossible msg
	  | _ => ErrorMsg.impossible msg
    end

fun storefloat(src,dst) =
    let val msg = "vax/vax/storefloat: bad source register"
    in case src of
	displace(_, base) => if base = fp_base 
		             then (V.movg(src, defer dataptr);
				   V.movl(immed(desc_reald),displace(~4,dataptr'));
				   V.movl(dataptr,dst);
				   V.addl2(immed(4*3), dataptr))
			     else ErrorMsg.impossible msg
      | _ => ErrorMsg.impossible msg
    end
	      

fun realop f (op1 as displace(_,b1),op2 as displace(_,b2),op3 as displace(_,b3)) =
    if b1 <> fp_base orelse b2 <> fp_base orelse b3 <> fp_base 
    then ErrorMsg.impossible "vax/vax/realop: Bad registers to float operator"
    else f (op1,op2,op3)

val fmuld = realop V.mulg3
val fdivd = realop (fn (a,b,c) => V.divg3 (b,a,c))
val faddd = realop V.addg3
val fsubd = realop (fn (a,b,c) => V.subg3 (b,a,c))
fun fnegd(src as displace(_,b1),dst as displace(_,b2)) = 
    if b1<>fp_base andalso b2<>fp_base then
	ErrorMsg.impossible "VaxCM.fnegd"
    else V.mnegg(src,dst)
fun fabsd (args as (displace(_,b1), displace(_,b2))) =
    if b1<>fp_base andalso b2<>fp_base
      then ErrorMsg.impossible "VaxCM.fabsd"
      else let
	val lab = V.newlabel()
	in
	  V.movg args;  (* movg sets condition codes *)
	  V.bgeq (address lab);
	  V.mnegg args;
	  V.define lab
	end

fun cvti2d(src, dst as displace(_,base)) = 
    if base <> fp_base then 
	ErrorMsg.impossible "VaxCM.cvti2d"
    else 
	V.cvtwg(src,dst)

fun cbranch NEQ = V.bneq
  | cbranch EQL = V.beql
  | cbranch LEQ = V.bleq
  | cbranch GEQ = V.bgeq
  | cbranch LSS = V.blss
  | cbranch GTR = V.bgtr

fun ibranch (cond, op1, op2, label) =
	(V.cmpl(op1, op2); cbranch cond (defer label))

(* rangeChk (a, b, lab):  pc <- lab if ((a < 0) or (b <= a)) *)
fun rangeChk (op1, op2, label) = (V.cmpl(op1, op2); V.bgequ (defer label))

fun fbranchd (cond, op1, op2, label) =
    (V.cmpg(op1, op2); cbranch cond (defer label))

fun defer' j = fn x => j(defer x)
val jmp = defer' V.jmp
val bbs = fn(x,y,l) => V.bbs(x,y, defer l)

val comment = V.comment
end
