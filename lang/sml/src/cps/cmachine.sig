(* cmachine.sig
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories
 *)

signature CMACHINE =
  sig

  (* effective addresses.  It is assumed that the following kinds exist:
   *   register;
   *   immediate label:  
   *      This mode doesn't in fact exist on the Vax or the MC68020,
   *      but it can be simulated (e.g. by "move address" instructions).
   *   immediate integer literal;
   *)
    type EA

    val immed : int -> EA		(* makes the immediate integer mode *)

  (* DEDICATED REGISTERS *)
  (* The following registers can hold pointers or properly tagged integers *)
    val exnptr : EA	    	(* the current exception-handler *)

  (* The following may be a register (indexable or not) or a memory location. *)
    val varptr: EA	        (* the var array *)
    val varptr_indexable : bool (* true iff varptr is an indexable register *)

  (* The following register may not hold pointers, and may hold untagged ints *)
    val arithtemps : EA list
      (* If arithtemps are present, then every attempt must be made to do 
       * "arithmetic" operations in arithtemp and arithtemp2 rather than
       * general registers.  This is all for the 68020.  Note that arithtemps
       * on that machine are not capable of all the operations
       * that "general" registers can do, and vice versa.  
       *)

  (* The following registers are not dedicated, and must be all disjoint *)
    val standardlink : EA
    val standardclosure : EA
    val standardarg : EA
    val standardcont : EA
    val miscregs : EA list
    val floatregs : EA list
    val savedfpregs : EA list

    val move : EA * EA -> unit  (* move(a,b)    a -> b *)

    val align : unit -> unit  (* ensure that next code is on 4-byte boundary *)
    val mark: unit -> unit    (* insert a gc-tag in the code so that next address
			       * may be moved into a record *)
    val emitlong : int -> unit (* put an 4-byte integer literal into the code *)
    exception BadReal of string
    val realconst : string -> unit  (* put a floating literal into the code *)
    val emitstring : string -> unit (* put a literal string into the code
				   (just the chars, no descriptor or length) *)
    val emitlab : int * EA -> unit  (* L3: emitlab(k,L2) is equivalent to
				   L3: emitlong(k+L2-L3) *)

    val newlabel : unit -> EA	(* create a new label (but don't define it) *)
    val define : EA -> unit  (* Associate a label with a point in the code *)

  (* checkLimit (n,lab,mask):
   * Generate code to check the heap limit to see if there is enough free space
   * to allocate n bytes.  "mask" shows what miscregs are live.
	"lab" is a label or register holding the resumption point.
   *)
    val testLimit: unit -> unit
    val checkLimit : int * EA * EA -> unit

    val beginStdFn : EA * EA -> unit;  
	      (* Note the beginning of a standard function; useful for machines
		 that don't have PC-relative instructions and have
                 to reset a base register.  First argument is a label,
	         second is a register pointing to that label. *)

    val jmp : EA -> unit	  (* unconditional jump to the address specified *)

    val record : (EA * CPS.accesspath) list * EA -> unit
		 (* makes a new record, puts address of it
		    into the destination specified by the second arg.
		    The contents are numbered from ~1 and up. *)

  (* recordStore(x, y, alwaysBoxed) records a store operation into mem[x+2*(z-1)].
   * The flag alwaysBoxed is true if the value stored is guaranteed to be boxed.
   *)
    val recordStore : (EA * EA * bool) -> unit

    val select : int * EA * EA -> unit  (* select(i,x,y) = y <- mem[x+4*i] *)
    val offset : int * EA * EA -> unit  (* offset(i,x,y) = y <- x+4*i *)

  (* fetchindexb(x,y,z) fetches a byte: y <- mem[x+z], where y is not x or z *)
    val fetchindexb : EA * EA * EA -> unit
  (* storeindexb(x,y,z) stores a byte: mem[y+z] <- x. *)
    val storeindexb : EA * EA * EA -> unit

    val jmpindexb : EA*EA -> unit	    (* jmpindexb(x,y)    pc <- (x+y) *)

  (* fetchindexl(x,y,z) fetches a word: y <- mem[x+2*(z-1)] *)
    val fetchindexl : EA * EA * EA -> unit   
  (* storeindexl(x,y,z) stores a word: mem[y+2*(z-1)] <- x *)
    val storeindexl : EA * EA * EA -> unit   
	    					
  (* fetchindexd(x,y,z): y<-mem[x+4*(z-1)] *)
    val fetchindexd : EA * EA * EA -> unit 
  (* storeindexd(x,y,z): mem[y+4*(z-1)]<-x *)
    val storeindexd : EA * EA * EA -> unit 

    val ashl : EA * EA * EA -> unit  (* shift left: count, src, dest;
				     shift count is non-negative *)
    val ashr : EA * EA * EA -> unit  (* shift right: count, src, dest;
				     shift count is non-negative *)
	   
    val orb :  EA * EA * EA -> unit  (* bitwise or *)
    val andb :  EA * EA * EA -> unit  (* bitwise and *)
    val xorb :  EA * EA * EA -> unit  (* bitwise xor *)
    val notb :  EA * EA -> unit  (* bitwise complement *)

    val add : EA * EA * EA -> unit  (* add(a,b,c):   c <- b+a *)
    val sub : EA * EA * EA -> unit	(* sub(a,b,c):  c <- (b - a) *)


  (* integer arithmetic with overflow trapping *)
    val addt : EA * EA * EA -> unit
    val subt : EA * EA * EA -> unit	(* subt(a,b,c):  c <- (b - a) *)
    val mult : EA * EA -> unit
    val divt : EA * EA -> unit		(* divt(a,b):  b <- (b div a) *)

    val bbs	  : EA * EA * EA -> unit

    datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

  (* ibranch (cond, a, b, lab):  pc <- lab if (a <cond> b). *)
    val ibranch : condition * EA * EA * EA -> unit

  (* rangeChk (a, b, lab):  pc <- lab if ((a < 0) or (b <= a)) *)
    val rangeChk : EA * EA * EA -> unit

  (* double precision floating point arithmetic.  These take FP registers
   * as their arguments.
   *)
    val fmuld : EA * EA * EA -> unit
    val fdivd : EA * EA * EA -> unit
    val faddd : EA * EA * EA -> unit
    val fsubd : EA * EA * EA -> unit
    val fnegd : EA * EA -> unit
    val fabsd : EA * EA -> unit
  (* convert an int to a double *)
    val cvti2d : EA * EA -> unit	(* ea1=gpr, ea2=fpr *)
  (* double test and branch *)
    val fbranchd : condition * EA * EA * EA -> unit
  (* load/store double precision floating point registers *)
    val storefloat : EA * EA -> unit	(* ea1=fpr, ea2=gpr *)
    val loadfloat: EA * EA -> unit	(* ea1=gpr, ea2=fpr *)

    val comment : string -> unit

  end (* CMACHINE *)
