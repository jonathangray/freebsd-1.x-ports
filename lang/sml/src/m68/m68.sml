(* Copyright 1989 by AT&T Bell Laboratories *)
functor M68CM(V : M68CODER) : CMACHINE = struct

structure V' : sig 
			exception BadReal of string
			datatype Register = DataReg of int
					  | AddrReg of int
					  | FloatReg of int
					  | PC
			
		        type Label sharing type Label = V.Label
			datatype Size = Byte | Word | Long
			
			datatype EA = Direct of Register
				    | PostInc of Register
				    | PreDec of Register
				    | Displace of Register * int
				    | Index of Register * int * Register * Size
				    | Immed of int
				    | Immedlab of Label
				    | Abs of int
				    | Address of Label

		end = V
open V'

datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

(* +DEBUG
fun diag (s : string) f x =
	f x handle e =>
		(print "?exception "; print (System.exn_name e);
		 print " in m68."; print s; print "\n";
		 raise e)
-DEBUG *)

fun defer(Direct r) = Displace(r,0)
  | defer(Immedlab lab) = Address lab
  | defer _ = ErrorMsg.impossible "defer in cpsm68"

(* DEBUG val defer = diag "defer" defer *) 

val exnptr = Direct(DataReg 7)
val varptr = Direct(DataReg 3)
val varptr_indexable = false
val dataptr as Direct dataptr' = Direct(AddrReg 6)
val arithtemps = map (Direct o DataReg) [0,1]
val arithtemp2 = Direct(DataReg 2)
val arithtemp3 = Direct(DataReg 4)
val storeptr = Direct(DataReg 6)
val standardclosure = Direct(AddrReg 2)
val standardarg = Direct(AddrReg 0)
val standardcont = Direct(AddrReg 1)
val standardlink = Direct(AddrReg 3)
val miscregs = map (Direct o AddrReg) [4]
val savedfpregs = map (Direct o FloatReg) [2,3,4,5,6,7]
val floatregs = [Direct(FloatReg 0), Direct(FloatReg 1)]
val fptempreg = Direct(FloatReg 7)
val datalimit = Direct(DataReg 5)

val threeaddress = false

val ptrtemp as Direct ptrtemp' = Direct(AddrReg 5)

fun reg(Direct r) = r

fun newlabel() = Immedlab(V.newlabel())
(* DEBUG val newlabel = diag "newlabel" newlabel *) 
fun emitlab(i,Immedlab lab) = V.emitlab(i,lab)
fun define (Immedlab lab) = V.define lab

(* stuff for simulating PC relative addressing (not used on M68) *)
fun beginStdFn _ = ()

val align = V.align
val mark = V.mark
fun move(src as Direct(FloatReg _),dst as Direct(FloatReg _)) = V.fmovex(src,dst)
  | move(src as Direct(FloatReg _), _) = ErrorMsg.impossible 
    "m68/m68/move: Destination not a floating point register"
  | move(_, dst as Direct(FloatReg _)) = ErrorMsg.impossible 
    "m68/m68/move: Source not a floating point register"
  | move(Immedlab l, dest as Direct(AddrReg x)) = V.lea(Address l, dest)
  | move(Immedlab l, dest) = 
	    (V.lea(Address l, ptrtemp);
	     move(ptrtemp,dest))
  | move(Displace(DataReg(d), i), dest) =
	    (V.movl(Direct(DataReg(d)), ptrtemp);
	     move(Displace(reg(ptrtemp), i), dest))
(* let's hope that ptrtemp never shows up in both src and dest! *)
  | move(src, Address l) =
	      (V.lea(Address l, ptrtemp);
	       move(src, Displace(reg(ptrtemp), 0)))
  | move x = V.movl x
(* DEBUG val move = diag "move" move *) 

fun testLimit() = V.cmpl(dataptr,datalimit)

(* checkLimit (n):
 * Generate code to check the heap limit to see if there is enough free space
 * to allocate n bytes.
 *)
fun checkLimit (maxAllocSize, lab, mask) =
   let val lab' = V.newlabel()
    in V.comment ("check limit, max alloc = "^(makestring maxAllocSize)^"\n");
       if maxAllocSize >= 4096
	   then (V.movl (dataptr, arithtemp3);
		 V.addl(Immed(maxAllocSize-4096), arithtemp3);
		 V.cmpl(arithtemp3, datalimit))
	   else ();
       V.jgt(Address lab');
       move(mask, arithtemp3);
       move(lab,ptrtemp);
       V.rts();
       V.define lab'
   end


val emitlong = V.emitlong
val realconst = V.realconst
val emitstring = V.emitstring

fun ashl(s as Immed k, r, d as Direct(DataReg _)) =
    (if r<>d then move(r,d) else (); 
     if k>8 then (move(s, arithtemp3);
		  V.asll(arithtemp3, d))
	    else V.asll(s,d))
  | ashl(s as Direct(DataReg _),r,d as Direct(DataReg _)) =
     if r<>d then if s<>d then (move(r,d);
				V.asll(s,d))
		          else (move(r,arithtemp3);
				V.asll(s,arithtemp3);
				move(arithtemp3,d))
	     else V.asll(s,d)
  | ashl(s as Direct(DataReg _),r,d) =
    (move(r,arithtemp3); V.asll(s,arithtemp3); move(arithtemp3,d))
  | ashl(s as Immed k,r,d) =
    let fun f(k) = if k=0 then () 
		    else if k>8 then (V.asll(Immed 8,arithtemp3);
				      f(k-8))
			 else V.asll(Immed k, arithtemp3)
     in move(r,arithtemp3);
        f k;
        move(arithtemp3,d)
    end

(* DEBUG val ashl = diag "ashl" ashl *) 

fun ashr(s as Immed k, r, d as Direct(DataReg _)) =
    (if r<>d then move(r,d) else (); 
     if k>8 then (move(s, arithtemp3);
		  V.asrl(arithtemp3, d))
	    else V.asrl(s,d))
  | ashr(s as Direct(DataReg _),r,d as Direct(DataReg _)) =
     if r<>d then if s<>d then (move(r,d);
				V.asrl(s,d))
		          else (move(r,arithtemp3);
				V.asrl(s,arithtemp3);
				move(arithtemp3,d))
	     else V.asrl(s,d)
  | ashr(s as Direct(DataReg _),r,d) =
    (move(r,arithtemp3); V.asrl(s,arithtemp3); move(arithtemp3,d))
  | ashr(s as Immed k,r,d) =
     let fun f(k) = if k=0 then () 
		    else if k>8 then (V.asrl(Immed 8,arithtemp3);
				      f(k-8))
			 else V.asrl(Immed k, arithtemp3)
     in move(r,arithtemp3);
	f k;
        move(arithtemp3,d)
    end

(* DEBUG val ashr = diag "ashr" ashr *) 

fun jmpindexb(lab,Direct r) = V.jra(Index(PC,2,r,Byte))
  | jmpindexb _ = ErrorMsg.impossible "bad args to jmpindexb in m68.sml"
(* DEBUG val jmpindexb = diag "jmpindexb" jmpindexb *) 

fun record(vl, z) =
    let open CPS
	fun f (Direct r, SELp(j,p)) = f(Displace(r,j*4),p)
	  | f (Immedlab l, p) = (V.lea(Address l, ptrtemp); f(ptrtemp,p))
	  | f (x,OFFp 0) = V.movl(x, PostInc dataptr')
	  | f (Direct r, OFFp j) = (V.lea(Displace(r,j*4),ptrtemp);
				    f(ptrtemp,OFFp 0))
	  | f (x,p) = (V.movl(x,ptrtemp); f(ptrtemp,p))
      in
	 app f vl;
	 V.lea(Displace(dataptr',~4*(List.length(vl)-1)),z)
     end

  (* recordStore(x, y, alwaysBoxed) records a store operation into mem[x+2*(z-1)].
   * The flag alwaysBoxed is true if the value stored is guaranteed to be boxed.
   *)
    fun recordStore (x, y, _) = record ([
	    (Immed(System.Tags.make_desc(3, System.Tags.tag_record)), CPS.OFFp 0),
	    (x, CPS.OFFp 0), (y, CPS.OFFp 0), (storeptr, CPS.OFFp 0)
	  ], storeptr)

fun select(i, Direct r, s) = move(Displace(r,i*4),s)
  | select(0, a, s) = move(defer a,s)

fun offset(i, Direct r, s) = V.lea(Displace(r,i*4),s)
(* DEBUG val select = diag "select" select *) 
(* DEBUG val offset = diag "offset" offset *) 

exception Three
fun three opcode (a as Direct(AddrReg _), b as Direct(AddrReg _),
		  c as Direct(AddrReg _)) =
                (three opcode(a,b,arithtemp3); move(arithtemp3,c))
  | three opcode (a,b,c) = 
	    if b=c then opcode(a,b) 
	    else if a=c then (move(a,arithtemp3); three opcode(arithtemp3,b,c))
	    else (move(b,c); opcode(a,c))

fun threet opcode (a,b,c as Direct(AddrReg _)) =
                (threet opcode(a,b,arithtemp3); move(arithtemp3,c))
  | threet opcode (a,b,c) = 
	    if b=c then (opcode(a,b); V.trapv())
	    else if a=c then (move(a,arithtemp3); threet opcode(arithtemp3,b,c))
	    else (move(b,c); opcode(a,c); V.trapv())

fun three' opcode (a as Immed _,b,c as Direct(DataReg _)) =
	    three opcode(a,b,c)
  | three' opcode (a as Direct(AddrReg _),b,c) =
	    (move(b,arithtemp3); move(a,arithtemp2);
	     opcode(arithtemp2,arithtemp3); move(arithtemp3,c))
  | three' opcode (a,b,c) =
	    (move(b,arithtemp3); opcode(a,arithtemp3); move(arithtemp3,c))

fun orb(a as Immed k,b,c as Direct(DataReg _)) =
    if k<65538
    then if k<=0
         then raise Match
	 else if b=c then V.orl(a,b) else (move(b,c); V.orl(a,c))
    else (move(a,arithtemp3);
	  if b=c then V.orl(arithtemp3,b) else (move(b,c); V.orl(arithtemp3,c)))
  | orb(a as Direct(AddrReg _),b,c) =
	    (move(b,arithtemp3); move(a,arithtemp2);
	     V.orl(arithtemp2,arithtemp3); move(arithtemp3,c))
  | orb(a as Immed k,b,c) =
    if k<65536
    then if k<=0
         then raise Match
	 else (move(b,arithtemp3); V.orl(a,arithtemp3); move(arithtemp3,c))
    else (move(a,arithtemp2);
	  move(b,arithtemp3);
	  V.orl(arithtemp2,arithtemp3);
	  move(arithtemp3,c))
  | orb(a,b,c) = (move(b,arithtemp3); V.orl(a,arithtemp3); move(arithtemp3,c))

fun xorb(a as Immed k,b,c as Direct(DataReg _)) =
    if k<65536
    then if k<=0
         then raise Match
	 else if b=c then V.eorl(a,b) else (move(b,c); V.eorl(a,c))
    else (move(a,arithtemp3);
	  if b=c then V.eorl(arithtemp3,b) else (move(b,c); V.eorl(arithtemp3,c)))
  | xorb(a as Direct(AddrReg _),b,c) =
	    (move(b,arithtemp3); move(a,arithtemp2);
	     V.eorl(arithtemp2,arithtemp3); move(arithtemp3,c))
  | xorb(a as Immed k,b,c) =
    if k<65538
    then if k<=0
         then raise Match
	 else (move(b,arithtemp3); V.eorl(a,arithtemp3); move(arithtemp3,c))
    else (move(a,arithtemp2);
	  move(b,arithtemp3);
	  V.eorl(arithtemp2,arithtemp3);
	  move(arithtemp3,c))
  | xorb(a,b,c) = (move(b,arithtemp3); V.eorl(a,arithtemp3); move(arithtemp3,c))

fun notb(a,b) = (move(Immed ~1,arithtemp3); V.subl(a,arithtemp3);
		 move(arithtemp3,b))
val andb = three' V.andl
val add = three V.addl
val addt = threet V.addl
val op sub = three V.subl
val subt = threet V.subl
fun mult x = (V.mull x; V.trapv())
val divt = V.divl  (* bug?  need to test for overflow or divide-by-zero? *)

exception Fetchindexb
(* fetchindexb(x,y,z) fetches a byte: y <- mem[x+z], *)
fun fetchindexb (Direct x, y, z) = (
      move(Immed 0, arithtemp3);
      case z
       of (Immed indx) => V.movb(Displace(x, indx), arithtemp3)
        | (Direct indx) => V.movb(Index(x, 0, indx, Byte), arithtemp3);
      move(arithtemp3,y))
(* storeindexb(x,y,z) stores a byte: mem[y+z] <- x. *)
fun storeindexb (x as Direct(AddrReg _), y, i) = 
    (move(x,arithtemp3);
     storeindexb(arithtemp3, y, i))
  | storeindexb (x, Direct y, Direct indx) = V.movb(x, Index(y, 0, indx, Byte))
  | storeindexb (x, Direct y, Immed indx) = V.movb(x, Displace(y, indx))
(* DEBUG val fetchindexb = diag "fetchindexb" fetchindexb *)
(* DEBUG val storeindexb = diag "storeindexb" storeindexb *)

fun fetchindexl(Direct x,y,Immed k) = move(Displace(x,k+k-2),y)
  | fetchindexl(Direct x,y,Direct z) = move(Index(x,~2,z,Word),y)
  | fetchindexl(Immedlab lab, y, Direct z) = 
	    (* this is a hack, since it depends on lab being PC+6 *)
		    move(Index(PC,4,z,Word), y);
(* DEBUG val fetchindexl = diag "fetchindexl" fetchindexl *) 
fun storeindexl(x, y, Immed 1) = move(x, defer y)
  | storeindexl(x, Direct y, Immed k) = move(x, Displace(y,k+k-2))
  | storeindexl(x, Direct y, Direct z) = move(x,Index(y,~2,z,Word))
(* DEBUG val storeindexl = diag "storeindexl" storeindexl *) 

fun float f (fp1,fp2,fp3) = 
    if fp2 <> fp3 then 
	(V.fmovex(fp1,fp3); f(fp2,fp3))
    else (V.fmovex(fp1,fptempreg); f(fp2,fptempreg); V.fmovex(fptempreg,fp3))

fun loadfloat(src,dst) = case dst 
			   of Direct(FloatReg fp) => V.fmoved(defer src, dst)
			    | _ => ErrorMsg.impossible
				   "m68/m68/loadfloat: Bad destination register"
fun storefloat (src as Direct(FloatReg fp), dst) = (
      V.movl(Immed(System.Tags.desc_reald), PostInc dataptr');
      V.movl(dataptr, dst);
      V.fmoved(src, PostInc dataptr'))
  |storefloat  _ = ErrorMsg.impossible "m68/m68/storefloat: Bad source register"
	  

val fmuld = float V.fmulx
val fdivd = float V.fdivx
val faddd = float V.faddx
val fsubd = float V.fsubx

fun fnegd (src as Direct(FloatReg _), dst as Direct(FloatReg _)) = V.fnegx(src, dst)
  | fnegd _ = ErrorMsg.impossible "m68/m68/fnegd"

fun fabsd (src as Direct(FloatReg _), dst as Direct(FloatReg _)) = V.fabsx(src, dst)
  | fabsd _ = ErrorMsg.impossible "m68/m68/fabsd"

fun cvti2d (src, dst as Direct(FloatReg _)) = V.fmovel(src, dst)
  | cvti2d _ = ErrorMsg.impossible "m68/m68/cvti2d"

					(* y <- mem[x+4*(z-1) *)
fun fetchindexd(Direct x', y as Direct(FloatReg fp), z) = 
    (case z 
      of Immed i => V.fmoved(Displace(x',4*(i-1)), y)
       | Direct z' => V.fmoved(Index(x',~4,z',Long), y)
       | _ => ErrorMsg.impossible "m68/m68/fetchindexd: bad index")
  | fetchindexd _ = ErrorMsg.impossible "m68/m68/fetchfloat: bad dest. reg"

					(* mem[y+4*(z-1)] <- x *)
fun storeindexd(x as Direct(FloatReg fp), Direct y', z) = 
    (case z 
      of Immed i => V.fmoved(x,Displace(y',4*(i-1)))
       | Direct z' => V.fmoved(x,Index(y',~4,z',Long))
       | _ => ErrorMsg.impossible "m68/m68/storeindexd: bad index")
  | storeindexd _ = ErrorMsg.impossible "m68/m68/storeindexd: bad src reg"

fun cbranch NEQ = V.jne
  | cbranch EQL = V.jeq
  | cbranch LEQ = V.jle
  | cbranch GEQ = V.jge
  | cbranch LSS = V.jlt
  | cbranch GTR = V.jgt

fun fbranch NEQ = V.fjne
  | fbranch EQL = V.fjeq
  | fbranch LEQ = V.fjle
  | fbranch GEQ = V.fjge
  | fbranch LSS = V.fjlt
  | fbranch GTR = V.fjgt

fun rev LEQ = GEQ
  | rev GEQ = LEQ
  | rev LSS = GTR
  | rev GTR = LSS
  | rev NEQ = NEQ
  | rev EQL = EQL

fun ibranch (cond, op1 as Immed _, op2, label) =
	(V.cmpl(op1, op2); cbranch (rev cond) (defer label))
  | ibranch (cond, op1, op2, label) =
	(V.cmpl(op2, op1); cbranch cond (defer label))

(* rangeChk (a, b, lab):  pc <- lab if ((a < 0) or (b <= a)) *)
fun rangeChk (op1 as Immed _, op2, label) = (V.cmpl(op1, op2); V.jls (defer label))
  | rangeChk (op1, op2, label) = (V.cmpl(op2, op1); V.jcc (defer label))
(* DEBUG val rangeChk = diag "rangeChk" rangeChk *)

fun fbranchd (cond, op1, op2, label) = (V.fcmpx(op2,op1); 
				       fbranch cond (defer label))

fun jmp x = V.jra(defer x)
(* DEBUG val jmp = diag "jmp" jmp *) 

fun bbs (x,dest as Direct(AddrReg _) ,l) = (move(dest,arithtemp2);
					    bbs(x,arithtemp2,l))
  | bbs (x,y,l) = (V.btst(x,y); V.jne(defer l))
(* DEBUG val bbs = diag "bbs" bbs *) 

val immed = Immed

val comment = V.comment
end
