(* Copyright 1989 by AT&T Bell Laboratories *)
functor CPSgen(M: CMACHINE) :
  sig structure CPS : CPS
    val codegen : CPS.function list * 
	          (CPS.lvar -> {known: Limit.kind, words_alloc: int}) *
	          ErrorMsg.complainer 
		     -> unit
  end =
struct
open Array List
infix 9 sub

fun dispose x = ()

val die = ErrorMsg.impossible

structure CPS = CPS
open M System.Tags Access CPS
val op sub = Array.sub
structure CG = System.Control.CG

val globalvar : EA option = NONE  (* obsolete *)


datatype RegType = FPReg | GPReg

(* FPR(fp,gp) => the variable is in the floating register fp _only_,
 *		 with an allocated general register gp
 * DPR(fp,gp) => the variable is in _both_ a floating register fp,
 *		 and the general register gp.
 * GPR gp     => the variable is n a general register gp only.
 *)          
datatype Reg = GPR of int		(* general purpose reg *)
	     | FPR of (int * int) 	(* floating reg * shadow gp reg *)
             | DPR of (int * int)	(* dual regs (fpr,gpr) *)

datatype generated = UNGEN of lvar list * cexp | GEN of lvar list * Reg list

datatype frag
  = STANDARD of (lvar * lvar list * cexp) option ref * int
  | KNOWN of generated ref
  | KNOWN_CHECK of generated ref * int
  | STRINGfrag of string
  | REALfrag of string
fun regtype2string rty = case rty of FPReg => "FPReg " | GPReg => "GPReg "
fun reg2string reg = case reg 
    of FPR(fp,gp) => "FPR(" ^ makestring fp ^ "," ^ makestring gp ^ ")"
     | DPR(fp,gp) => "DPR(" ^ makestring fp ^ "," ^ makestring gp ^ ")"
     | GPR gp => "GPR(" ^ makestring gp ^ ")"
exception GpregNum and FpregNum and ShadowNum
fun gpregNum reg = case reg 
		     of GPR gp => gp
		      | DPR(_,gp) => gp
		      | FPR _ => raise GpregNum
fun fpregNum reg = case reg 
		     of FPR(fp,_) => fp
		      | DPR(fp,_) => fp
		      | GPR _ => raise FpregNum
fun shadowNum reg = case reg
		      of FPR(fp,gp) => gp
		       | DPR(fp,gp) => gp
		       | GPR _ => raise ShadowNum

val allregs = standardlink::standardclosure::standardarg::standardcont::miscregs

val allfpregs = M.savedfpregs @ M.floatregs
local 
    exception FPRegEA and GPRegEA
    val gpregarr = arrayoflist allregs
    val fpregarr = arrayoflist allfpregs
in 
fun fpregEA reg = (fpregarr sub (fpregNum reg)) handle _ => raise FPRegEA
fun gpregEA reg = (gpregarr sub (gpregNum reg)) handle _ => raise GPRegEA
end
val max_fp_parameters = let val len = length M.savedfpregs
			in case M.floatregs 
			     of [] => if len=0 then 0 else len-1
			      | _ => len
			end
fun collect(_,[]) = []
  | collect(pred,x::xs) = 
    if pred x then x :: collect(pred,xs) else collect(pred,xs)

structure GetScratch :
    sig
	exception GetFpScratch
	exception GetGpScratch
	val getfpscratch: int * Reg list -> int
	val getgpscratch: int * Reg list -> int
	val arithtemp : EA
	val fpregtemp : EA
	val resetGetscratch : unit->unit
    end =
struct
val ok_gpreg = array(length allregs, true)
val ok_fpreg = array(length allfpregs, true)
val last_gp = ref 0
val last_fp = ref 0
fun resetGetscratch () = (last_gp := 0; last_fp := 0)
val len_gp = Array.length ok_gpreg
val len_fp = Array.length ok_fpreg
fun mark b reg = 
    (*
     * ~1 may be passed as a don't care register number,
     * hence the handle Subscript ..
     *)
    let fun mboth(fp,gp) =
	(update(ok_fpreg,fp,b) handle Subscript => ();
	 update(ok_gpreg,gp,b) handle Subscript => ())
    in case reg
	 of GPR i => (update(ok_gpreg,i,b) handle Subscript => ())
	  | FPR(fp,gp) => mboth(fp,gp)
	  | DPR(fp,gp) => mboth(fp,gp)
    end
fun mark_prohibited proh = map (mark false) proh

fun cleanup regs = map (mark true) regs
exception FindReg
fun find_reg(okregs, next) =
    let fun find i = if okregs sub i then i else find (i+1)
	fun find2 i = if okregs sub i then i
		      else if i=next then raise FindReg else find2(i+1)
    in  find next handle Subscript => find2 0
    end

exception GetScratch
fun getregscratch(pref, proh, okregs, last, len) = 
    (mark_prohibited proh;
     (if ((okregs sub pref) handle Subscript => false) then pref
      else (find_reg(okregs, !last) 
	        handle FindReg => (cleanup proh; raise GetScratch)))
	  before
	  (cleanup proh;
	   last := (if !last+1=len then 0 else !last+1)))

exception GetFpScratch
fun getfpscratch(pref,proh) = 
    (getregscratch(pref, proh, ok_fpreg, last_fp, len_fp)
     	handle GetScratch => raise GetFpScratch)

exception GetGpScratch
fun getgpscratch(pref,proh) = 
    (getregscratch(pref, proh, ok_gpreg, last_gp, len_gp) 
       	handle GetScratch => raise GetGpScratch)

val arithtemp = case arithtemps 
		  of z::_ => z 
		   | _ => let val r = GPR(length(miscregs)+3)
                           in mark false r; gpregEA r
                          end
val fpregtemp = 
    (* see also max_fp_parameters *)
    case allfpregs 
      of [] => ErrorMsg.impossible "cps/generic: no floating point registers"
       | _ => let val tmp_reg = length allfpregs - 1
	       in  mark false (FPR (tmp_reg,~1)); 
		   fpregEA (FPR(tmp_reg, ~1))
	       end
end
open GetScratch

fun codegen(funs : (lvar * lvar list * cexp) list, limits, err) =
let 

    fun fromto(i,j) = if i > j then nil else ((GPR i)::fromto(i+1,j))
    val k = !CG.calleesaves
    val calleesaveregs = fromto(4,k+3)
    fun standardformals args = 
       let val t = length(args)
        in if t = (k+4) then ([GPR 0,GPR 1,GPR 2,GPR 3]@calleesaveregs)
           else if t = (k+2) then (GPR 3 :: GPR 2 :: calleesaveregs)
                else if ((t = 3) andalso (k = 0)) then [GPR 0,GPR 3,GPR 2]
                     else ErrorMsg.impossible "110 in CPSgen"
       end

    val _ = resetGetscratch()

    exception Labbind
    val labtable : EA Intmap.intmap = Intmap.new(32, Labbind)
    val addlabbinding = Intmap.add labtable
    val labmap = Intmap.map labtable

    exception Know
    val knowtable : frag Intmap.intmap = Intmap.new(32, Know)
    val addknow = Intmap.add knowtable
    val know = Intmap.map knowtable

    exception Freemap
    val freemaptable : lvar list Intmap.intmap = Intmap.new(32, Freemap)
    val freemap = Intmap.map freemaptable
    val cexp_freevars = FreeMap.cexp_freevars freemap
 
    exception Regbind
    val regbindtable : Reg Intmap.intmap = Intmap.new(32, Regbind)
    val addregbinding = Intmap.add regbindtable
    val regmap = Intmap.map regbindtable

    fun fpregbind_exists var = case regmap var of GPR _ => false | _ => true
    fun gpregbind_exists var = case regmap var of FPR _ => false | _ => true
	
    exception GpregMap and FpregMap
    fun gpregmap var = 
	if gpregbind_exists var then regmap var else raise GpregMap
    fun fpregmap var = 
	if fpregbind_exists var then regmap var else raise FpregMap
	
    fun clean (VAR x::r) = x :: clean r
      | clean (_::r) = clean r
      | clean [] = []
    fun live_regs(args:lvar list) = map regmap args
    fun makefrag (f,vl,e) =
	    let val lab = newlabel()
		val knowledge = 
		    case limits f
		     of {known=Limit.ESCAPES, words_alloc = alloc} => 
			    STANDARD(ref(SOME(f,vl,e)), alloc)
		      | {known=Limit.KNOWN, ...} => 
			    KNOWN(ref(UNGEN(vl,e)))
		      | {known=Limit.KNOWN_CHECK, words_alloc = alloc} => 
			    KNOWN_CHECK(ref(UNGEN(vl,e)), alloc)
	    in  addknow(f, knowledge); 
		addlabbinding(f,lab);
		(lab,knowledge)
	    end
    val frags = ref(map makefrag funs)
    val _ = (dispose(limits,"limits"); dispose(makefrag,"makefrag");
	     dispose(funs,"funs"))
    fun addfrag f = frags := f :: !frags

    exception Strings
    local open IntStrMap
          val m : EA intstrmap = new(32,Strings)
    in fun enterString (s,lab) = add m (StrgHash.hashString(s),s,lab);
       fun lookString s = map m (StrgHash.hashString(s),s)
    end

    fun regbind(VAR v,regtype) = 
	(*
	 * Returns a register binding for a cps value
	 * A write back is generated when a integer register binding is
	 * required for a value in a floating register.
	 *)
	let val reg = regmap v
	in case (reg,regtype)
	     of (FPR(fp,gp),GPReg) => 
		 let val newreg = DPR(fp,gp)
		 in storefloat(fpregEA newreg, gpregEA newreg);
		     addregbinding(v, newreg);
		     gpregEA newreg
		 end
	      | (_,GPReg) => gpregEA reg
	      | (_,FPReg) => fpregEA reg
	end
      | regbind(LABEL v, GPReg) = labmap v
      | regbind(INT i, GPReg) = (immed(i+i+1) handle Overflow =>
		       ErrorMsg.impossible "Overflow in cps/generic.sml")
      | regbind(STRING s, GPReg) =
           (lookString s handle Strings =>
			  let val lab = newlabel()
			  in addfrag(lab, STRINGfrag s);
			      enterString(s,lab);
			      lab
			  end)
      | regbind(REAL s, GPReg) = let val lab = newlabel()
				 in  addfrag(lab, REALfrag s);
				     lab
				 end
      | regbind(OBJECT _, GPReg) = ErrorMsg.impossible "OBJECT in cps/generic/regbind"
      | regbind(_, FPReg) =
	ErrorMsg.impossible "value not loaded into floating register"

    val gpregbind : value -> EA = fn x => regbind(x,GPReg)
    val fpregbind : value -> EA = fn x => regbind(x,FPReg)

    exception RegMask
    fun regmask formals =
	let fun f (i,mask) =
	    case i
	      of GPR gp => Bits.orb(Bits.lshift(1,gp),mask)
	       | FPR _ => mask
	       | DPR _ => raise RegMask
	in immed(fold f formals 0)
	end


(* add advanced register-targeting, currently the targeting depth is 4 if
 * calleesaves > 0.
 *  root1 : (lvar * ((lvar * Reg) list)) ref
 *  root : cexp -> lvar * ((lvar * Reg) list)
 *)

    val ss : int ref = ref(!CG.targeting)  (* global var *)

    fun merge2((v1,r1)::t1,(v2,r2)::t2) = 
           if v1 = v2 then ((v1,r1)::merge2(t1,t2))
           else if v1 < v2 then ((v1,r1)::merge2(t1,(v2,r2)::t2))
                else ((v2,r2)::merge2((v1,r1)::t1,t2))
      | merge2(nil,t2) = t2
      | merge2(t1,nil) = t1

    fun union2((f,t1),(_,t2)) = (f,merge2(t1,t2))

    fun mix(t1,t2) = 
      let fun mix0((VAR v)::tl,r::rl) = (v,r)::mix0(tl,rl)
            | mix0(_::tl,r::rl) = mix0(tl,rl)
            | mix0(nil,nil) = nil
            | mix0 _ = ErrorMsg.impossible "error in cps/generic/mix0"
          val op slt = fn ((i,_),(j,_)) => (i < (j:int))
       in Sort.sort (op slt) (mix0(t1,t2))
      end

    fun gprfilter(f,tinfo) = 
      let fun g((v,GPR r)::tl) = (v,GPR r)::(g tl)
            | g((v,_)::tl) = (g tl)
            | g nil = nil
       in (f,g tinfo)
      end

    fun targeting(wl,vl,e) = 
      if !ss = 0 then nil 
      else (let val olds = !ss
                val _ = (ss := olds-1)
                val (_,tinfo) = root(e)
                val _ = (ss := olds)

                fun findv(v,nil) = NONE
                  | findv(v,(w,r)::tl) = if v = w then (SOME r) else findv(v,tl)

                fun extract(nil,nil) = nil
                  | extract((VAR w)::wl,v::vl) = 
                      (case findv(v,tinfo) of 
                          NONE => extract(wl,vl)
                        | (SOME r) => merge2([(v,r)],
                                       (merge2([(w,r)],extract(wl,vl)))))
                  | extract(_::wl,v::vl) =
                      (case findv(v,tinfo) of 
                          NONE => extract(wl,vl)
                        | (SOME r) => merge2([(v,r)],extract(wl,vl)))
                  | extract _ = ErrorMsg.impossible "errors in cps/generic/extract"
             in extract(wl,vl)
            end)
    and getroot(APP(VAR f,wl)) = (f,mix(wl,standardformals wl))
      | getroot(APP(LABEL f,wl)) = 
         (case know f 
            of KNOWN(ref(GEN(vl,fmls))) => (f,mix(wl@(map VAR vl),fmls@fmls))
	     | KNOWN(ref(UNGEN(vl,e))) => 
                   (let val tmp = targeting(wl,vl,e)
                     in (f,tmp)
                    end)
	     | KNOWN_CHECK(ref(GEN(vl,fmls)),_) => (f,mix(wl@(map VAR vl),fmls@fmls))
	     | KNOWN_CHECK(ref(UNGEN(vl,e)),_) => 
                   (let val tmp = targeting(wl,vl,e)
                     in (f,tmp)
                    end)
	     | STANDARD _ => (f,mix(wl,standardformals wl))
	     | _ => die "a10 in CPSgen")

      | getroot _ = ErrorMsg.impossible "errors in cps/generic/getroot"
    and root(RECORD(_,_,_,e)) = root e
      | root(SELECT(_,_,_,e)) = root e
      | root(OFFSET(_,_,_,e)) = root e
      | root(SWITCH(_,_,el)) = fold union2 (map root el) (0,nil)
      | root(SETTER(_,_,e)) = root e
      | root(LOOKER(_,_,_,e)) = root e
      | root(ARITH(_,_,_,e)) = root e
      | root(PURE(_,_,_,e)) = root e
      | root(BRANCH(_,_,_,e1,e2)) = union2 (root e1, root e2)
      | root(e as APP _) = gprfilter (getroot e)
      | root _ = die "a9 in CPSgen"
    val root1 :(lvar * ((lvar * Reg) list)) ref = ref((0,nil))

    fun nextuse x =
	let fun xin[] = false 
	      | xin(VAR y::r) = x=y orelse xin r 
	      | xin(_::r) = xin r
	    fun g(level,a) =
		let val rec f =
		    fn ([],[]) => level
		     | ([],next) => g(level+1,next)
		     | (SWITCH(v,_,l)::r,next) => 
			   if xin[v] then level else f(r,l@next)
		     | (RECORD(_,l,w,c)::r,next) =>
			   if xin(map #1 l) then level 
			   else f(r,c::next)
		     | (SELECT(i,v,w,c)::r,next) => 
			   if xin[v] then level else f(r,c::next)
		     | (OFFSET(i,v,w,c)::r,next) => 
			   if xin[v] then level else f(r,c::next)
		     | (SETTER(i,a,c)::r,next) => 
			   if xin a then level else f(r,c::next)
		     | (LOOKER(i,a,w,c)::r,next) => 
			   if xin a then level else f(r,c::next)
		     | (ARITH(i,a,w,c)::r,next) => 
			   if xin a then level else f(r,c::next)
		     | (PURE(i,a,w,c)::r,next) => 
			   if xin a then level else f(r,c::next)
		     | (BRANCH(i,a,c,e1,e2)::r,next) => 
			   if xin a then level else f(r,e1::e2::next)
		     | (APP(v,vl)::r,next) => 
			   if xin(v::vl) then level 
			   else f(r,next)
		     | _ => die "a8 in CPSgen"
		in f(a,[])
		end
	    fun h y = g(0,[y])
	in h
	end

    fun next_fp_use(x,cexp) : int option = 
	let val there = exists (fn VAR x' => x=x'| _ => false)
	    fun fp_use(SETTER(P.updatef,[_,_,VAR x'],_)) = x'=x
	      | fp_use(ARITH(P.fadd,vl,_,_)) = there vl

	      | fp_use(ARITH(P.fdiv,vl,_,_)) = there vl
	      | fp_use(ARITH(P.fmul,vl,_,_)) = there vl
	      | fp_use(ARITH(P.fsub,vl,_,_)) = there vl
	      | fp_use(BRANCH(P.fge,vl,_,_,_)) = there vl
	      | fp_use(BRANCH(P.fgt,vl,_,_,_)) = there vl
	      | fp_use(BRANCH(P.fle,vl,_,_,_)) = there vl
	      | fp_use(BRANCH(P.flt,vl,_,_,_)) = there vl
	      | fp_use(BRANCH(P.feql,vl,_,_,_)) = there vl
	      | fp_use(BRANCH(P.fneq,vl,_,_,_)) = there vl
	      | fp_use _ = false
	    fun f (cexp,level) =
	        case cexp
		  of RECORD(_,_,_,ce) => f(ce,level+1)
		   | SELECT(_,_,_,ce) => f(ce,level+1)
		   | OFFSET(_,_,_,ce) => f(ce,level+1)
		   | APP _ => NONE
		   | FIX _ => ErrorMsg.impossible "FIX in generic.sml"
		   | SWITCH(_,_,cl) => fpop_in_all_branches(cl,level)
		   | SETTER(_,_,ce) => f(ce,level+1)
		   | LOOKER(_,_,_,ce) => f(ce,level+1)
		   | PURE(_,_,_,ce) => f(ce,level+1)
		   | ARITH(_,_,_,ce) => if fp_use cexp then SOME level
					 else f(ce,level+1)
		   | BRANCH(_,_,_,c1,c2) => if fp_use cexp then SOME level
				      else fpop_in_all_branches([c1,c2],level)
	    and
		fpop_in_all_branches (branches,level) =
		let val all_branches =  map (fn c => f(c,level)) branches
		in if exists (fn opt => opt = NONE) all_branches  
		       then NONE
		   else let val lvls = map (fn SOME l => l
					     | _ => die "a8 in CPSgen") 
		                           all_branches
			in SOME (fold min lvls (hd lvls))
			end
		end
	in f(cexp,0)
	end

    fun preferred_register_asgn(formals,cexp) =
	if max_fp_parameters=0 then map (fn _ => GPReg) formals

	else 
          let val preferred_regs =  
	          map (fn SOME x => (FPReg,x) | NONE => (GPReg, 0))
		      (map (fn v => next_fp_use(v,cexp)) formals)
	      fun assign([],_) = []
		| assign(xs,0) = map (fn _ => GPReg) xs
		| assign((GPReg,_)::xs,acc) = GPReg::assign(xs,acc)
		| assign((FPReg,lvl)::xs,acc) =
		  let fun better_params([],c) = c
			| better_params((FPReg,lvl')::xs, c) =
			  if lvl' < lvl then better_params(xs,c+1)
			  else better_params(xs,c)
			| better_params(_::xs,c) = better_params(xs,c)
		  in if better_params(xs,0) >= acc then GPReg::assign(xs,acc)
		     else FPReg::assign(xs,acc-1)
		  end
	  in assign(preferred_regs, max_fp_parameters)
	  end

    val any = INT 0 (* default argument for alloc *)

    fun alloc(v,default,continue) =
	(*
	 * allocate a general purpose register for the new
	 * free variable v, and continue.
	 *)
	let val (f,tinfo) = !root1

	    val proh = live_regs (freemap v)
	    fun delete (z,nil) = nil
	      | delete (z:Reg, a::r) = if a=z then delete(z,r) else (a::delete(z,r))
	    val default = case default
			    of VAR i => ((gpregmap i) handle GpregMap => GPR ~1)
			     | _ => GPR ~1
	    fun get(good,bad) =
		let val r = getgpscratch(gpregNum good,bad@proh) 
		            handle GetGpScratch => 
        			          getgpscratch(gpregNum default,proh)
				 | GpregNum => 
				          getgpscratch(gpregNum default, proh)
		in addregbinding(v,GPR r); continue(gpregEA (GPR r))
		end
	    fun find tinfo =
		let fun g((w,r)::tl) =
		        if w=v then get(r, delete(r,map #2 tinfo))
			else g tl
		      | g _ = get(default, map #2 tinfo)
		in g tinfo
		end
	in find tinfo
	end

    fun partition_args(args:value list, formals:Reg list) =
	(*
	 * Moves registers to the right register class.
	 * This makes it easier to shuffle later.
	 * If an actual argument is required in both a floating and
	 * general register then it will end up in a DPR register.
	 *
	 * The process is split into 3 phases.
	 * 1. move_GPR_args moves arguments into GPRegs that do not have
	 *   a GPReg binding.
	 * 2. flush_fpregs removes all unnecessary bindings in 
	 *   floating registers.
	 * 3. move_FPR_args moves arguments into FPRegs.
	 *)
	let fun move_GPR_args(VAR var::vs, GPR gp::fmls) = 
	        if gpregbind_exists var then  move_GPR_args(vs,fmls)
		else let val FPR(fp,gp) = regmap var
			 val newreg = DPR(fp,gp)
		     in (*
			 * Use shadow register to store floating value.
			 *)
			 storefloat(fpregEA newreg, gpregEA newreg);
			 addregbinding(var,newreg);
			 move_GPR_args(vs,fmls)
		     end
	      | move_GPR_args (_::a,_::f) = move_GPR_args(a,f)
	      | move_GPR_args ([],[]) = ()
	      | move_GPR_args _ =
		ErrorMsg.impossible "cps/generic/partition_args/move_GPR_args"
	    fun flush_fpregs () =
	        let open SortedList
		    fun GPRonly_args() =
		      let val pairs = List2.map2 (fn x => x) (args,formals)
			  val inFPRegs = 
			      collect(fn (VAR _,FPR _) => true | _ =>false,pairs)
			  val inGPRegs = 
			      collect(fn (VAR _,GPR _)=> true | _ =>false,pairs)
			  val h = fn (VAR x,_) => x | _ => die "a7 in CPSgen"
		      in difference (uniq(map h inGPRegs),uniq(map h inFPRegs))
		      end
		    fun f (d::ds) = 
			let val reg = regmap d 
			in case reg 
			     of DPR(fp,gp) => 
				 (* release floating point register *)
				 (addregbinding(d, GPR gp); f ds)
			      | GPR _ => f ds
			      | FPR _ => ErrorMsg.impossible
				    "cps/generic/partition_args/flush_fpregs"
			end
		      | f [] = ()
		in f (GPRonly_args())
		end
	    val formal_fp_regs = 
		let fun f (r::regs) = 
		    (case r 
		       of FPR(fp,_) => fp::f regs 
			| DPR(fp,_) => ErrorMsg.impossible
		                      "cps/generic/partition_args/formal_fp_regs"
			| _ => f regs)
		      | f [] = []
		in f formals
		end
	    fun move_FPR_args(VAR v::vs, (FPR(fp,_)::fmls)) =
		let fun getfpreg pref =
		        (* 
			 * The preferred floating register is the corresponding
			 * formal floating register, so this is deleted from
			 * the formals in a first attempt at getting a floating
			 * register.
			 *)
		        let fun delete (_,[]) = []
			      | delete (r,r'::rest) = if r=r' then rest 
						      else r'::delete(r,rest)
			    val liveregs = live_regs (clean args)
			    val avoid = map (fn r => FPR(r,~1)) 
				            (delete(pref,formal_fp_regs))
			in getfpscratch(pref, liveregs@avoid)
			       handle GetFpScratch => 
				         getfpscratch(pref,liveregs)
			end
		in if fpregbind_exists v then move_FPR_args(vs,fmls)
		   else let val z = getfpreg fp
			    val r = gpregNum (regmap v)
			    val newreg = DPR(z,r)
			in loadfloat(gpregEA newreg, fpregEA newreg);
			    addregbinding(v, newreg);
			    move_FPR_args(vs,fmls)
			end
		end
	      | move_FPR_args(_::a,_::f) = move_FPR_args(a,f)
	      | move_FPR_args([],[]) = ()
	      | move_FPR_args _ = 
		ErrorMsg.impossible "cps/generic/partition_args/move_FPR_args"
	in  move_GPR_args(args, formals);
	    if exists (fn FPR _ => true | _ => false) formals then
		(flush_fpregs (); move_FPR_args(args, formals))
	    else ()
	end

    fun shuffle_regtype(args:value list,formals:Reg list,regtype:RegType) =
	(*
	 *  Move actual arguments into registers for function call.
	 * Assumes that all the variable actuals have a binding in 
	 * the correct register class.
	 * If an actual is being passed in a floating and general 
	 * register, then its binding must be a DPR register.
	 * The function shuffles register of a specific type
	 * i.e. FPReg, GPReg.
	 *)
	let val (tempreg,EAfcn,regnum) =
		case regtype of GPReg => (arithtemp,gpregEA,gpregNum)
                              | FPReg => (fpregtemp,fpregEA,fpregNum)
	    fun classify(VAR v::al, f::fl, match, nomatch, notinreg) =
		let val v' = regmap v
		in if regnum v' = regnum f
		       then classify(al,fl,regnum f::match,nomatch,notinreg)
		   else classify(al,fl,match,(v',f)::nomatch,notinreg)
		end
	      | classify(a::al,f::fl, m,n,notinreg) = 
 		classify(al,fl,m,n,(a,f)::notinreg)
	      | classify(_,_, m,n,nr) = (m,n,nr)
	    fun f (pairs,used) = 
	      let val u' = (map (regnum o #1) pairs) @ used
		  fun movable (a, b) = not (exists (fn z => z = regnum b) u')
		  fun split pred nil = (nil,nil)
		    | split pred (a::r) =
		      let val (x,y) = split pred r
		      in if pred a then (a::x, y) else (x, a::y)
		      end
	      in case split movable pairs
		   of (nil,_) => (pairs,used)
		    | (m,m') => (app (fn(a,b)=>move(EAfcn a, EAfcn b)) m;
				 f(m', (map (regnum o #2) m) @ used))
	      end
            fun cycle(pairs, used) =
		case f(pairs,used)
		  of (nil,_) => ()
		   | ((a,b)::r, used) =>
			 cycle(move(EAfcn a, tempreg);
			       f(r,used) before move(tempreg, EAfcn b))
	    val (matched,notmatched,notinreg) = classify(args,formals,[],[],[])
	in
          cycle(notmatched,matched);
	  app (fn (a,b) => 
	          case regtype
		    of GPReg => move(gpregbind a, EAfcn b)
		     | FPReg => loadfloat(gpregbind a, EAfcn b))
	      notinreg
	end

    fun do_shuffle(args:value list,formals:Reg list) =
	(*
	 * - Partitions the actual arguments into sets
	 * based on their destination class.
	 * i.e. All agruments headed for general registers are
	 * in one partition, and all arguments headed for floating
	 * registers in another.
	 *)
	let fun register_sets(v::vs,f::fs,gv,gf,fv,ff) =
		(case f 
		    of GPR _ => register_sets(vs,fs,v::gv,f::gf,fv,ff)
		     | FPR _ => register_sets(vs,fs,gv,gf,v::fv,f::ff)
		     | DPR _ => ErrorMsg.impossible "cps/generic/do_shuffle")
	      | register_sets([],[],gv,gf,fv,ff) = ((gv,gf,GPReg),(fv,ff,FPReg))
	      | register_sets _ = ErrorMsg.impossible "register_sets/do_shuffle"
	
	    val _ = partition_args(args,formals)
	    val (gp_set,fp_set) = register_sets(args,formals,[],[],[],[])
	in shuffle_regtype gp_set;
	   shuffle_regtype fp_set
	end

    fun allocparams(args:value list, formals:lvar list, prefer:RegType list) =
       (* 
	* Determines the parameter passing convention for a function.
        * This is complicated by the fact that an actual may not be in the
	* appropriate register class.
	* Even if an actual is in the correct register class it may not
	* be in a suitable register, since only a specific set of registers
	* can be used for parameter passing.
	* Precondition: 
	* 	|formals| <= maxfree && |live_regs(clean args)| <= maxfree
	* Invariant pass1:
	* 	|live_regs(clean(args)| + used_gp <= maxfree
	*) 
	let open SortedList
	    datatype PRegs = 
		OK of Reg 	(* allocated general registers *)
	      | NO of int	(* allocated shadow register for a float *)
	    val liveregs = live_regs (clean args)
	    fun getgpr avoid =
		getgpscratch(~1, liveregs @ (map GPR avoid)) 
		handle GetGpScratch =>
		 (System.Print.say "allocparams\n";
		  raise GetGpScratch)
	    fun okFPR_param fpr = fpr < max_fp_parameters
	    fun inuse (reg,already) = exists (fn r => r=reg) already
            val (_,tinfo) = !root1
            fun findv v = 
               let fun g((w,r)::tl) = (if w = v then (SOME (gpregNum r)) else g(tl))
                     | g nil = NONE
                in g tinfo
               end

	    (* pass1 is guided by the preferred register class.
	     * If an actual is in the right register class and has not been 
	     * assigned then it is marked as being assigned.
	     * Otherwise an allocation is made.
	     * The shadow register is used where appropriate. 
	     *)
	    fun pass1 (VAR v::vl, p::pref, used_gp, used_fp,u::ul) =
		(case p
		   of GPReg =>
		       let fun test_gp_reg z =
			       if inuse(z,used_gp) 
                               then (case (findv u) of 
                                       NONE => (getgpr used_gp)
                                     | (SOME r) => if inuse(r,used_gp) 
                                                   then getgpr used_gp
                                                   else r) 
			       else z
			   fun pass1_with_gpreg z =
			       let val w = test_gp_reg z
			       in OK(GPR w)::pass1(vl,pref,w::used_gp,used_fp,ul)
			       end
			   val reg = regmap v
		       in if gpregbind_exists v 
			  then pass1_with_gpreg(gpregNum reg)
			  else pass1_with_gpreg(shadowNum reg)
		       end
		    | FPReg =>
		      let fun bad_fpreg gp =
			      let val r = if not(inuse(gp,used_gp)) then gp
					  else getgpr used_gp
			      in NO r:: pass1(vl,pref,r::used_gp,used_fp,ul)
			      end
			  val reg = regmap v
		      in if fpregbind_exists v then
			    let val z = fpregNum reg
				val r = shadowNum reg
			    in if okFPR_param z andalso 
				  not (inuse(z,used_fp)) andalso
				  not (inuse(r,used_gp))
			       then OK(FPR(z,r))::
				    pass1(vl,pref,r::used_gp,z::used_fp,ul)
			       else bad_fpreg r
			    end
			 else bad_fpreg (gpregNum reg)
		      end)
	      | pass1 (_::vl, p::pref, used_gp, used_fp, u::ul) =
		let val z = (case (findv u) of
                               NONE => (getgpr used_gp) 
                             | (SOME w) => (if inuse(w,used_gp) 
                                            then getgpr used_gp
                                            else w))
		in (case p of GPReg => OK(GPR z) | FPReg => NO z) ::
		    pass1(vl,pref,z::used_gp,used_fp,ul)
		end
	      | pass1 ([],[],_,_,[]) = []
	      | pass1 _ =
		ErrorMsg.impossible "cps/generic/allocparams/pass1"
	    fun assigned_FPregs assgm =
		map (fn OK(FPR(fp,_)) => fp
		      | _ => die "a6 in CPSgen")
		    (collect(fn OK(FPR(fp,_))=> true | _ => false,assgm))
	    fun pass2 asgm =
		let val savedFPRegs = 
		        let fun from (n,m) = 
			        if n >= m then [] else n::from(n+1,m)
			in from (0, max_fp_parameters)
			end
		    val unusedfpr = difference (uniq savedFPRegs,
						uniq (assigned_FPregs asgm))
		    fun pass2(NO gp::pregs,fp::fpregs)=
			FPR(fp,gp)::pass2(pregs,fpregs)
		      | pass2(NO _ ::pregs, []) =
			ErrorMsg.impossible "cps/generic/allocparams/pass2"
		      | pass2(OK reg::pregs, fpregs) = reg :: pass2(pregs,fpregs)
		      | pass2 ([],_) = []
		in pass2(asgm, unusedfpr)
		end
	    val assign1 = pass1(args,prefer,[],[],formals)
	    val final = if exists (fn rty => rty = FPReg) prefer
		        then pass2 assign1
			else map (fn (OK r) => r
				   | _ => die "a5 in CPSgen") assign1
	in
	    
	    List2.app2 addregbinding (formals,final);
	    do_shuffle(args, final);
	    final
	end

    fun stupidargs(f,args,vl,pref) = 
	(*
	 * - assign integer and floating registers in sequence
	 * starting from 0.
	 *)
	let fun argregs(v::rest,p::pref,gpreg,fpreg) =
	        (case p 
		   of GPReg => 
		       (addregbinding(v,GPR gpreg);
			GPR gpreg::argregs(rest,pref,gpreg+1,fpreg))
		    | FPReg => 
		       let val newreg = FPR(fpreg,gpreg)
		       in addregbinding(v,newreg);
			   newreg::argregs(rest,pref,gpreg+1,fpreg+1)
		       end)
	      | argregs ([],_,_,_) = []
	      | argregs _ = ErrorMsg.impossible "cps/generic/stupidargs"
	    val formals = argregs(vl,pref,0,0)
	in  do_shuffle(args,formals); formals
	end

    fun force_fpgetscratch (pref:int, proh:Reg list, cexp) =
	(*
	 * - allocate a floating point registers spilling if necessary.
	 * The floating registers in proh cannot be spilled.
	 * All free variables in cexp must have a register binding.
	 *)
      let val free = cexp_freevars cexp
	  exception Spill
	  fun spill():lvar =
	      let fun find_fp_spill_reg [] = raise Spill
		    | find_fp_spill_reg ((_,v)::uv) = 
		      if fpregbind_exists v
                      then let val r = fpregmap v
			   in if exists (fn reg => fpregNum reg = fpregNum r) 
			                proh
			      then find_fp_spill_reg uv
			      else v
			   end
		      else find_fp_spill_reg uv
		  val sortdecreasing = 
		      Sort.sort (fn ((i:int,_),(j:int,_)) => i < j)
		  val uses = map (fn v =>(nextuse v cexp, v)) free
	      in find_fp_spill_reg (sortdecreasing uses)
	      end
	  fun duplicates(vl:lvar list) =
	      let val avoid = 
		      (map fpregNum proh) handle RegNum => ErrorMsg.impossible
			  "cps/generic/force_getfpscratch/duplicates"
		  fun bad_dup v = exists (fn r => v = r) avoid
		  fun f (x::xs) = 
		      let val r = regmap x
		      in case r 
			   of DPR (fp,gp) => 
			       if bad_dup fp then f xs else (x,fp,gp)::f xs  
			    | _ => f xs
		      end
		    | f [] = []
	      in f vl
	      end
	  fun pref_dup [] = NONE
	    | pref_dup ((a as (v,fp,gp))::ds) =
	      if fp = pref then SOME a else pref_dup ds

	  exception FirstNoneUse of lvar
	  fun find_good_dup dups = 
	      let val sort = 
		  Sort.sort (fn ((_,lvl1),(_,lvl2)) => lvl1 <= lvl2)
		  val f = (fn (v,fp,gp) => case next_fp_use(v,cexp) 
					     of NONE => raise FirstNoneUse v
					      | SOME lvl => (v,lvl))
	      in #1 (hd (sort (map f dups)))
	      end

	  fun nofpr_handle () =
	      let val dups = duplicates free
	      in 
		  case pref_dup dups
		    of SOME(v,fp,gp) => 
			(addregbinding(v,GPR gp); fp)
		     | NONE => 
			if null dups then
			    let val z = (spill() handle Spill => 
					 		   raise GetFpScratch)
				val r as FPR(fp,gp) = fpregmap z
				val newreg = GPR gp
			    in storefloat(fpregEA r, gpregEA newreg);
				addregbinding(z, newreg);
				fp
			    end 
			else
			    (*
			     * Find the dup that is not going to be used
			     * in a floating context or one that is
			     * going to be used the furthest away.
			     *)
			    let val v = (find_good_dup dups) 
						handle FirstNoneUse x => x
				val DPR(fp,gp) = regmap v
			    in addregbinding(v, GPR gp); fp
			    end
	      end
      in  getfpscratch (pref, proh @ live_regs free)
	  handle GetFpScratch => (nofpr_handle ())
      end

    exception MoveToFPRs
    fun move_to_FPRs(vl, cexp) =
	(*
	 * move variables in vl to floating registers.
	 *)
	let fun f (VAR x::r,moved) =
		if fpregbind_exists x then f(r, regmap x::moved)
		else let val fp = force_fpgetscratch(~1,moved,cexp)
			 val gp = gpregNum(regmap x)
			 val newreg = DPR(fp,gp)
		     in loadfloat(gpregEA newreg,fpregEA newreg);
			 addregbinding(x, newreg);
			 f(r, newreg::moved)
		     end
	      | f (a::r,moved) =
		(*
		 * There is never a register allocated for constants.
		 * So when moving constants into floating point registers
		 * we _must_ not allocate the shadow register.
		 *)
		let val fp = force_fpgetscratch(~1,moved,cexp)
		    val newreg = FPR(fp, ~1)
		in loadfloat(gpregbind a,fpregEA newreg);
		    f(r, newreg::moved)
		end
	      | f ([],moved) = rev moved
	in f(vl,[])
	end

    fun do_fp_primop (args,w,e,cexp,continue) =
	(* 
	 * ensure that the required arguments are in floating 
	 * registers and allocates a FPR for the result.
	 *)
	let
	    val moved  = move_to_FPRs(args,cexp)
	    val u = getgpscratch(~1, live_regs(freemap w))
	    (* 
	     * A lie to guarantee precondition for force_fpgetscratch
	     * which we promptly confess when creating newreg
	     *)
	    val _ = addregbinding(w, GPR u) 
	    val z = let 
			(* clean_fpregs:
			 * This function is required because of the M68k 
			 * that does not support 3 operand floating point 
			 * instructions. See definition of float in m68.sml
			 *
			 * Clean_fpregs removes the shadow registers in the
			 * moved set.
			 * Saying that they are prohibited is not strictly 
			 * correct. 
			 *)
			fun clean_fpregs [] = []
			  | clean_fpregs (x::xs) = 
			    (case x 
			       of FPR(fp,_) => FPR(fp, ~1) :: clean_fpregs xs
				| DPR(fp,_) => DPR(fp, ~1) :: clean_fpregs xs
				| GPR _ => ErrorMsg.impossible 
				            "cps/generic/do_fp_primop")
		    in force_fpgetscratch(~1, clean_fpregs moved, e)
		    end
	    val newreg = FPR(z,u)
	in  addregbinding(w,newreg);
	    continue (map fpregEA moved, fpregEA newreg)
	end

    fun tempreg(x,f) = case arithtemps of _::z::_ => f z | _ => f x

    fun genfrag (_, STANDARD(ref NONE,_)) = ()
      | genfrag (lab, STANDARD(r as ref (SOME(fname,fmls,e)), alloc)) =
	let val fmls' as linkreg::_ = standardformals fmls
	in  r := NONE;
  	    Intmap.clear regbindtable;
	    FreeMap.freemap (Intmap.add freemaptable) e;
	    List2.app2 addregbinding (fmls, fmls');
	    align(); mark();
	    comment(Access.lvarName fname ^ ":\n");
            define lab;
            beginStdFn(lab, gpregEA linkreg);
            checkLimit (alloc*4, gpregEA linkreg, regmask fmls');
            root1 := root e;
            gen e;
  	    Intmap.clear freemaptable
	end
      | genfrag (_, KNOWN _) = ()
      | genfrag (_, KNOWN_CHECK _) = ()
      | genfrag (lab, REALfrag r) = 
	(align(); 
	 mark(); 
	 emitlong(desc_embedded_reald);
	 define lab; 
	 comment("# real constant " ^ r ^ "\n");
	 realconst r
	 handle M.BadReal r =>
	   err ErrorMsg.COMPLAIN ("real constant out of range: " ^ r)
	     ErrorMsg.nullErrorBody)
      | genfrag (lab, STRINGfrag s) = 
	(align(); 
	 mark();
	 emitlong(make_desc(size s, tag_embedded_string));
	 define lab; 
	 emitstring s; 
	 align())

    (* generate a new code label *)
    and genlab(lab, cexp) = (root1 := root cexp; define lab; gen cexp)

    and parallel_gen (shared_vars, f1, f2) =
	let val bindings = map regmap shared_vars
	in f1(); 
	   List2.app2 addregbinding (shared_vars,bindings); 
	   f2()
	end

    and gen cexp =
	case cexp
	  of RECORD(k,vl,w,e) =>
	      alloc(w, any,  fn w' => let
		val desc = case (k, length vl)
		     of (RK_VECTOR, l) => make_desc(l, tag_record)
		      | (_, 2) => desc_pair
		      | (_, l) => make_desc(l, tag_record)
		in
		  record ((immed desc, OFFp 0)
		    :: map (fn(x,p)=>(gpregbind x, p)) vl, w');
		  gen e
		end)
          | SELECT(i,INT k,w,e) =>(* the generated code'll never be executed *)
              alloc(w,any, fn w' => (move(immed(k+k),w'); gen e))
          | APP(INT k,args) => () (* the generated code'll never be executed *)
	  | SELECT(i,v,w,e) =>
	      alloc(w,any, fn w' => (select(i,gpregbind v,w'); gen e))
	  | OFFSET(i,v,w,e) =>
	      alloc(w, v, fn w' => (offset(i,gpregbind v,w'); gen e))
	  | APP(func as VAR f, args) => 
		   let val formals as dest::_ = standardformals args
                    in do_shuffle(args,formals);
		       testLimit();
		       jmp(gpregEA dest)
		   end
	  | APP(func as LABEL f, args) =>
	    (case know f
	      of KNOWN(ref(GEN(_,formals))) =>
			   (do_shuffle(args, formals);
			    jmp(labmap f))
	       | KNOWN_CHECK(ref(GEN(_,formals)),_) =>
			   (do_shuffle(args, formals);
			    testLimit();
			    jmp(labmap f))
	       | KNOWN(r as ref(UNGEN(vl,cexp))) =>
		     let val _ =FreeMap.freemap (Intmap.add freemaptable) cexp;
			 val pref = if !CG.floatreg_params then
			 		preferred_register_asgn(vl,cexp)
				    else map (fn _ => GPReg) vl
			 val formals = if !CG.argrep then
			     		   allocparams(args,vl,pref)
				       else stupidargs(func,args,vl,pref)
			 val lab = labmap f
		     in r := GEN(vl,formals);
		     (*	 jmp lab;*)
			 comment(Access.lvarName f ^ ":\n");
                         define lab;
                         root1 := root cexp;
			 gen cexp before dispose(cexp,"known_cexp")
		     end
	       | KNOWN_CHECK(r as ref(UNGEN(vl,cexp)), alloc) =>
		     let val _ =FreeMap.freemap (Intmap.add freemaptable) cexp;
		         val pref = if !CG.floatreg_params then
			 		preferred_register_asgn(vl,cexp)
				    else map (fn _ => GPReg) vl
			 val formals = if !CG.argrep then
			     		   allocparams(args,vl,pref)
				       else stupidargs(func,args,vl,pref)
			 val lab = labmap f
		     in r := GEN(vl,formals);
			 testLimit();
			 jmp (lab); align(); mark();
			 comment(Access.lvarName f ^ ":\n");
                         define lab;
                         checkLimit (alloc*4, lab, regmask formals);
                         root1 := root cexp;
			 gen cexp
		     end
	       | k as STANDARD _ =>
		      (do_shuffle(args, standardformals args);
		       testLimit();
		       jmp(labmap f))
               | _ => die "a3 in CPSgen")
	  | APP _ => ErrorMsg.impossible "constant func in CPSgen"
	  | SWITCH(v,_,l) => 
		let val lab = newlabel()
		    val labs = map (fn _ => newlabel()) l;
		    fun f(i, s::r) = (emitlab(i, s); f(i+4, r))
		      | f(_, nil) = ()
		    fun h(lab::labs, e::es) = 
			parallel_gen(cexp_freevars e, 
				     fn () => genlab(lab,e),fn () => h(labs, es))
		      | h(nil,nil) = ()
		      | h _ = die "a4 in CPSgen"
		 in fetchindexl(lab, arithtemp, gpregbind v);
(*		    add(lab,arithtemp,arithtemp);
		    jmp(arithtemp); *)
		    jmpindexb(lab,arithtemp);
(*		    align();   temporarily removed so 68020 will work. *)
		    define lab;
		    f (0, labs);
		    h(labs,l)
		end
        | ARITH(P.+, [INT k, w],x,e) =>
	      alloc(x, w, fn x' =>
		    (addt(immed(k+k), gpregbind w, x');
		     gen e))
        | ARITH(P.+, [w, v as INT _],x,e) => gen(ARITH(P.+,[v,w],x,e))
        | ARITH(P.+, [v,w],x,e) =>
	      alloc(x, w, fn x' =>
		    (M.sub(immed 1, gpregbind v, arithtemp);
		     addt(arithtemp, gpregbind w, x');
		     gen e))
        | PURE(P.orb, [v,w],x,e) =>
          alloc(x, w, fn x' => (orb(gpregbind v, gpregbind w, x'); gen e))
        | PURE(P.andb, [v,w],x,e) =>
          alloc(x, w, fn x' =>(andb(gpregbind v, gpregbind w, x'); gen e))
        | PURE(P.xorb, [INT k, w],x,e) =>
	      alloc(x, w, fn x' =>
		    (xorb(immed(k+k), gpregbind w, x');
		     gen e))
        | PURE(P.xorb, [w,v as INT _],x,e) => gen(PURE(P.xorb,[v,w],x,e))
        | PURE(P.xorb, [v,w],x,e) =>
	      alloc(x,any, fn x' => tempreg(x', fn x'' =>
		    (xorb(gpregbind v, gpregbind w, x'');
		     add(immed 1, x'', x');
		     gen e)))
       | PURE(P.notb, [v],x,e) =>
          alloc(x, any, fn x' =>
	            (M.sub(gpregbind v, immed 0, x');
		     gen e))
       | PURE(P.lshift, [INT k, w],x,e) =>
	     alloc(x,w, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind w, x'');
		    ashl(x'',immed(k+k),x'');
		    add(immed 1, x'', x');
		    gen e)))
       | PURE(P.lshift, [v, INT k],x,e) =>
	     alloc(x,v, fn x' => tempreg(x', fn x'' =>
		   (add(immed ~1, gpregbind v, x'');
		    ashl(immed k, x'', x'');
		    add(immed 1, x'', x');
		    gen e)))
       | PURE(P.lshift, [v,w],x,e) =>
	     alloc(x,w, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind w, arithtemp);
		    add(immed ~1, gpregbind v, x'');
		    ashl(arithtemp, x'', x'');
		    add(immed 1, x'', x');
		    gen e)))
       | PURE(P.rshift, [v, INT k],x,e) =>
	     alloc(x, v, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed k, gpregbind v, x'');
		    orb(immed 1, x'', x');
 		    gen e)))
       | PURE(P.rshift, [v,w],x,e) =>
	     alloc(x, v, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind w, arithtemp);
		    ashr(arithtemp, gpregbind v, x'');
		    orb(immed 1, x'', x');
		    gen e)))
       | ARITH(P.-, [INT k,w],x,e) =>
	     alloc(x, w, fn x' =>
		   (M.subt(gpregbind w, immed(k+k+2), x');
		    gen e))
       | ARITH(P.-, [v, INT k],x,e) =>
	     alloc(x, v, fn x' =>
		   (M.subt(immed(k+k), gpregbind v, x');
		    gen e))
       | ARITH(P.-, [v,w],x,e) =>
	     alloc(x, v, fn x' => tempreg(x', fn x'' =>
		   (M.sub(gpregbind w, gpregbind v, x'');
		    add(immed 1, x'', x');
		    gen e)))
       | ARITH(P.*, [INT k, INT j],x,e) =>
	    alloc(x,any, fn x' => tempreg(x', fn x'' =>
	     (move(immed k, x'');
	      mult(immed(j+j),x'');
	      add(immed 1, x'', x');
	      gen e)))
       | ARITH(P.*, [INT 2,w],x,e) => gen(ARITH(P.+,[w,w],x,e))
(* Perhaps this isn't worth the trouble
       | ARITH(P.*, [INT 4,w],x,e) => 
	     let val v = mkLvar()
	      in gen(ARITH(P.+,[w,w],v,ARITH(P.+,[VAR v,VAR v],x,e)))
	     end
*)       | ARITH(P.*, [INT k, w],x,e) =>
	     alloc(x,any, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind w, x'');
		    mult(immed(k+k), x'');
		    add(immed 1, x'', x');
		    gen e)))
       | ARITH(P.*, [v,w as INT _],x,e) => gen(ARITH(P.*,[w,v],x,e))
       | ARITH(P.*, [v,w],x,e) =>
           alloc(x,any,fn x' => tempreg(x', fn x'' =>
	         (ashr(immed 1, gpregbind v, arithtemp);
		  M.sub(immed 1, gpregbind w, x'');
		  mult(arithtemp,x'');
		  add(immed 1,x'',x');
		  gen e)))
       | ARITH(P.div, [INT k, INT j],x,e) =>
	     alloc(x, any, fn x' => tempreg(x', fn x'' =>
		   (move(immed k, x'');
		    divt(immed j, x'');
		    addt(x'',x'',x'');
		    add(immed 1, x'',x');
		    gen e)))
       | ARITH(P.div, [INT k,w],x,e) =>
	     alloc(x, any, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind w, arithtemp);
		    move(immed k, x'');
		    divt(arithtemp,x'');
		    addt(x'',x'',x'');
		    add(immed 1, x'',x');
		    gen e)))
       | ARITH(P.div, [v, INT k],x,e) =>
	     alloc(x, any, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind v, x'');
		    divt(immed k, x'');
		    addt(x'',x'',x'');
		    add(immed 1, x'',x');
		    gen e)))
       | ARITH(P.div, [v,w],x,e) =>
            alloc(x,any, fn x' => tempreg(x', fn x'' =>
		  (ashr(immed 1, gpregbind w, arithtemp);
		   ashr(immed 1, gpregbind v, x'');
		   divt(arithtemp,x'');
		   addt(x'',x'',x'');
		   add(immed 1, x'',x');
		   gen e)))
       | LOOKER(P.!, [v],w,e) => gen (LOOKER(P.subscript, [v, INT 0], w, e))
       | ARITH(P.~, [v],w,e) =>
	 alloc(w,any,fn w' => (M.subt(gpregbind v,immed 2,w'); gen e))
       | PURE(P.makeref, [v],w,e) =>
	    alloc(w,any, fn w' =>
		(record([(immed(make_desc(1,tag_array)),OFFp 0),
			 (gpregbind v, OFFp 0)], w');
		 gen e))
       | BRANCH(P.lessu, [v,w],_,d,e) =>
	   let val false_lab = newlabel()
	    in rangeChk(gpregbind v, gpregbind w, false_lab);
		parallel_gen(cexp_freevars d,
			     fn () => gen d, 
			     fn () => genlab(false_lab, e))
	   end
       | BRANCH(P.gequ, [v,w],_,e,d) =>
	   let val false_lab = newlabel()
	    in rangeChk(gpregbind v, gpregbind w, false_lab);
		parallel_gen(cexp_freevars d,
			     fn () => gen d, 
			     fn () => genlab(false_lab, e))
	   end
       | LOOKER(P.subscript, [v,w],x,e) =>
			alloc(x,any, fn x' =>
			    (fetchindexl(gpregbind v, x', gpregbind w);
			     gen e))
       | PURE(P.subscriptv,[v,w],x,e) =>
	     alloc(x,any,fn x' => (fetchindexl(gpregbind v,x',gpregbind w);
				   gen e))
       | SETTER(P.update, [a, i, v], e) => let
	  val a' = gpregbind a and i' = gpregbind i
	  in
	    recordStore (a', i', false);
	    storeindexl (gpregbind v, a', i');
	    gen e
	  end
       | SETTER(P.boxedupdate, [a, i, v], e) => let
	  val a' = gpregbind a and i' = gpregbind i
	  in
	    recordStore (a', i', true);
	    storeindexl (gpregbind v, a', i');
	    gen e
	  end
       | SETTER(P.unboxedupdate, [a, i, v], e) =>
		(storeindexl(gpregbind v, gpregbind a, gpregbind i);
		 gen e)
       | PURE(P.length, [a as VAR _], x, e) =>  (* Note: least tag bit is 1 *)
	  alloc(x,any, fn x' => tempreg(x', fn x'' =>
	     (select(~1, gpregbind a, x'');
	      ashr(immed(width_tags-1), x'', x'');
	      move(x'',x');
	      gen e)))
       | PURE(P.length, [a], x, e) =>  (* Note: least tag bit is 1 *)
	  alloc(x,any, fn x' => tempreg(x', fn x'' =>
	     (move(gpregbind a, x');
	      select(~1,x',x'');
	      ashr(immed(width_tags-1), x'', x'');
	      move(x'',x');
	      gen e)))
       | PURE(P.objlength, [a], x, e) =>
	  alloc(x,any,  fn x' => tempreg(x', fn x'' =>
	    (select(~1, gpregbind a, x'');
	     ashr(immed(width_tags-1),x'', x'');
	     orb(immed 1, x'', x');
	     gen e)))
       | SETTER(P.store, [s,INT i', INT v'], e) =>
	     (storeindexb(immed v', gpregbind s, immed i');
	      gen e)
       | SETTER(P.store, [s,INT i',v], e) =>
	     (ashr(immed 1, gpregbind v, arithtemp);
	      storeindexb(arithtemp, gpregbind s, immed i');
	      gen e)
       | SETTER(P.store, [s,i,INT v'], e) =>
	     (ashr(immed 1, gpregbind i, arithtemp);
	      storeindexb(immed v', gpregbind s, arithtemp);
	      gen e)
       | SETTER(P.store, [s,i,v], e) =>
	     let val v' = gpregbind v
	      in ashr(immed 1, gpregbind i, arithtemp);
		 ashr(immed 1, v', v');
	         storeindexb(v', gpregbind s, arithtemp);
		 add(v',v',v');
		 add(immed 1, v', v');
		 gen e
	     end
       | LOOKER(P.ordof, [s as VAR _, INT k], v, e) =>
	     alloc(v,any, fn v' =>
		   (fetchindexb(gpregbind s, v', immed k);
		    add(v',v',v');
		    add(immed 1, v',v');
		    gen e))
       | LOOKER(P.ordof, [s, INT k], v, e) =>
	     alloc(v,any, fn v' =>
		   (move(gpregbind s, v');
		    fetchindexb(v', v', immed k);
		    add(v',v',v');
		    add(immed 1, v',v');
		    gen e))
       | LOOKER(P.ordof, [s as VAR _, i], v, e) =>
	     alloc(v,any, fn v' =>
		   (ashr(immed 1, gpregbind i, arithtemp);
		    fetchindexb(gpregbind s, v', arithtemp);
		    add(v',v',v');
		    add(immed 1, v',v');
		    gen e))
       | LOOKER(P.ordof, [s, i], v, e) =>
	     alloc(v,any, fn v' =>
		   (ashr(immed 1, gpregbind i, arithtemp);
		    move(gpregbind s, v');
		    fetchindexb(v', v', arithtemp);
		    add(v',v',v');
		    add(immed 1, v',v');
		    gen e))
       | BRANCH(P.boxed, [x],_,a,b) =>
		    let val lab = newlabel()
		     in bbs(immed 0,gpregbind x,lab); 
			 parallel_gen(cexp_freevars a,
				      fn () => gen a, 
				      fn () => genlab(lab,b))
		    end
       | BRANCH(P.unboxed, x,c,a,b) => gen(BRANCH(P.boxed,x,c,b,a))
       | LOOKER(P.gethdlr, [],x,e) =>
		  alloc(x,any, fn x' => (move(exnptr,x'); gen e))
       | SETTER(P.sethdlr, [x],e) => (move(gpregbind x, exnptr); gen e)
       | LOOKER(P.getvar, [], x, e0 as SETTER(primop, [VAR x',i,v], e)) =>
            if (varptr_indexable
	    andalso x=x' andalso not (SortedList.member (cexp_freevars e) x))
              then let
		val i' = gpregbind i
		in
		  case primop
		   of P.update => recordStore (varptr, i', false)
		    | P.boxedupdate => recordStore (varptr, i', true)
		    | P.unboxedupdate => ()
		    | _ => ErrorMsg.impossible "[CPSGen: varptr setter]"
		  (* end case *);
		  storeindexl(gpregbind v, varptr, i');
		  gen e
		end
	      else alloc(x,any, fn x' => (move(varptr,x'); gen e0))
       | LOOKER(P.getvar,[],x,
           e0 as LOOKER(P.subscript, [VAR x',y], w, e)) =>
            if varptr_indexable andalso
		 x=x' andalso not (SortedList.member (cexp_freevars e) x)
               then alloc(w,any, fn w' =>
			    (fetchindexl(varptr, w', gpregbind y);
			     gen e))
               else alloc(x,any, fn x' => (move(varptr,x'); gen e0))
       | LOOKER(P.getvar, [],x,e) =>
	          alloc(x,any, fn x' => (move(varptr,x'); gen e))
       | SETTER(P.setvar, [x],e) => (move(gpregbind x, varptr); gen e)
       | SETTER(P.uselvar, [x],e) => gen e
       | LOOKER(P.deflvar, [],x,e) => alloc(x,any, fn x' => gen e)
       | ARITH(P.fmul, vl, z, e) =>
	     do_fp_primop(vl,z,e,cexp, (fn ([x,y],z) => (fmuld(x,y,z); gen e)))
       | ARITH(P.fdiv, vl, z, e) =>
	     do_fp_primop(vl,z,e,cexp, (fn ([x,y],z) => (fdivd(x,y,z); gen e)))
       | ARITH(P.fadd, vl, z, e) =>
	     do_fp_primop(vl,z,e,cexp, (fn ([x,y],z) => (faddd(x,y,z); gen e)))
       | ARITH(P.fsub, vl, z, e) =>
	     do_fp_primop(vl,z,e,cexp, (fn ([x,y],z) => (fsubd(x,y,z); gen e)))
       | PURE(P.fnegd, vl, z, e) =>
	     do_fp_primop(vl,z,e,cexp, (fn ([x],y) => (fnegd(x,y); gen e)))
       | PURE(P.fabsd, vl, z, e) =>
	     do_fp_primop(vl,z,e,cexp, (fn ([x],y) => (fabsd(x,y); gen e)))
       | PURE(P.real,[v],w,e) => let
	   val gpr = getgpscratch (~1,live_regs(freemap w))
	   val _   = addregbinding (w,GPR gpr)
	   val fpr = force_fpgetscratch (~1,[],e)
	   val wreg = FPR (fpr,gpr)
	 in
	     addregbinding (w,wreg);
	     case v
	       of INT n => cvti2d(immed n, fpregEA wreg)
		| _ => (ashr(immed 1,gpregbind v,arithtemp);
			cvti2d(arithtemp,fpregEA wreg))
	     (* end case *);
	     gen e
	 end

(* still to implement: 
      floor | round *)

       | LOOKER(P.subscriptf,[a,i],w,e) =>
	     let val gp = getgpscratch(~1,live_regs(freemap w))
		 val _ = addregbinding(w, GPR gp)
		 val fp = force_fpgetscratch(~1,[],e)
		 val wreg = FPR(fp,gp)
	     in  addregbinding(w, wreg);
		 fetchindexd(gpregbind a, fpregEA wreg, gpregbind i);
		 gen e
	     end	
       | SETTER(P.updatef,[a,i,v],e) => 
	     let val a' = gpregbind a 
		 val i' = gpregbind i
		 val [fpreg] = move_to_FPRs([v],cexp)
	     in  storeindexd(fpregEA fpreg, gpregbind a, gpregbind i);
		 gen e
	     end
       | PURE(P.gettag, [v], x, e) =>
	    alloc (x, any, fn x' => tempreg(x', fn x'' => (
	      select(~1, gpregbind v, x'');
	      andb(immed(power_tags-1), x'', x'');
	      ashl(immed 1, x'', x'');
	      orb(immed 1, x'', x');
	      gen e)))
       | PURE(P.mkspecial, [INT i, v], w, e) =>
	    alloc(w, any, fn w' => (
	      record([
		(immed(make_desc(i, tag_special)), OFFp 0),
		(gpregbind v, OFFp 0)], w');
	      gen e))
       | PURE(P.mkspecial, [i, v], w, e) =>
	    alloc(w, any, fn w' => let
	      val i' = gpregbind i
	      in
		tempreg (i', fn i'' => (
		  ashr(immed(1), i', i'');
		  ashl(immed(width_tags), i'', i'');
		  orb(immed(desc_special), i'', i');
		  record([(i', OFFp 0), (gpregbind v, OFFp 0)], w');
		  gen e))
	      end)
       | LOOKER(P.getspecial, [v], x, e) =>
	    alloc (x, any, fn x' => tempreg(x', fn x'' => (
	      select(~1, gpregbind v, x'');
	      ashr(immed(width_tags-1), x'', x'');
	      orb(immed 1, x'', x');
	      gen e)))
       | SETTER(P.setspecial, [v, INT i], e) => (
	    storeindexl (immed(make_desc(i, tag_special)), gpregbind v, immed ~1);
	    gen e)
       | SETTER(P.setspecial, [v, i], e) => let
	    val i' = gpregbind i
	    in
	      tempreg (i', fn i'' => (
		ashr(immed(1), i', i'');
		ashl(immed(width_tags), i'', i'');
		orb(immed(desc_special), i'', i');
		storeindexl (i', gpregbind v, immed ~1);
		gen e))
	    end
       | BRANCH(args as (P.ieql,_,_,_,_)) => compare(ibranch,NEQ,args)
       | BRANCH(args as (P.ineq,_,_,_,_)) => compare(ibranch,EQL,args)
       | BRANCH(args as (P.>   ,_,_,_,_)) => compare(ibranch,LEQ,args)
       | BRANCH(args as (P.>=  ,_,_,_,_)) => compare(ibranch,LSS,args)
       | BRANCH(args as (P.<   ,_,_,_,_)) => compare(ibranch,GEQ,args)
       | BRANCH(args as (P.<=  ,_,_,_,_)) => compare(ibranch,GTR,args)
       | BRANCH(args as (P.feql,_,_,_,_)) => fpcompare(fbranchd,NEQ,cexp)
       | BRANCH(args as (P.fneq,_,_,_,_)) => fpcompare(fbranchd,EQL,cexp)
       | BRANCH(args as (P.fgt ,_,_,_,_)) => fpcompare(fbranchd,LEQ,cexp)
       | BRANCH(args as (P.flt ,_,_,_,_)) => fpcompare(fbranchd,GEQ,cexp)
       | BRANCH(args as (P.fge ,_,_,_,_)) => fpcompare(fbranchd,LSS,cexp)
       | BRANCH(args as (P.fle ,_,_,_,_)) => fpcompare(fbranchd,GTR,cexp)
       | _ => ErrorMsg.impossible "3312 in CPSgen"

    and compare(branch,test, (_,[v,w],_,d,e)) =
	let val lab = newlabel()
	in branch(test,gpregbind v, gpregbind w, lab); 
	    parallel_gen(cexp_freevars d,
			 fn () => gen d,
			 fn () => genlab(lab, e))
	end
      | compare _ = die "a1 in CPSgen"

    and
	fpcompare(branch, test, cexp as BRANCH(_,args as [v,w],_,d,e)) =
	let val lab = newlabel()
	    val reserved = move_to_FPRs([v,w], cexp)
	    val [v',w'] = reserved 
	in  branch(test, fpregEA v', fpregEA w', lab);
	    parallel_gen(cexp_freevars d,
			 fn () => gen d,
			 fn () => genlab(lab,e))
	end
      | fpcompare _ = die "a2 in CPSgen"

in  (* not necessary with regmasks: emitlong 1; Bogus tag for spacing, boot_v. *)
    let fun loop nil = ()
          | loop (frag::r) = (frags := r; genfrag frag; loop(!frags))
    in loop(!frags)
    end
end (* codegen *)

end (* structure *)
