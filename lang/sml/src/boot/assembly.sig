(* Copyright 1989 by AT&T Bell Laboratories *)

signature RUNVEC =
  sig
    val array : int * 'a -> 'a array
    val callc : 'b (* func*)  * 'a -> 'c
    eqtype bytearray
    eqtype realarray
    val create_b : int -> bytearray
    val create_r : int -> realarray
    val create_s : int -> string
    (*eqtype 'a vector*)
    val create_v : int * 'a list -> 'a vector
    val floor : real -> int
    val logb : real -> int
    val scalb : real * int -> real
    type spin_lock
    val try_lock : spin_lock -> bool
    val unlock : spin_lock -> unit
  end (* RUNVEC *)

signature ASSEMBLY =
  sig
    datatype datalist = DATANIL | DATACONS of (string * (unit -> unit) * datalist)
    type func
    datatype funclist = FUNCNIL | FUNC of (func * string * funclist)
    type object
    structure A : RUNVEC
    exception Div
    exception Overflow
    exception SysError of (int * string)
    exception UnboundTable
    val active_procs : int ref
    val array0 : 'a array
    val bytearray0 : A.bytearray
    val calleesaves : int
    val collected : int ref
    val collectedfrom : int ref
    val current : int ref
    val datalist : datalist
    val dtablesize : int
    val external : funclist
    val gcmessages : int ref
    val times : int array ref
    val lastratio : int ref
    val machine : string  (* m68, mipsb, mipsl, ns32, sparc or vax *)
    val majorcollections : int ref
    val minorcollections : int ref
    val opsys : int   (* 1 = vax bsd ultrix, 4.2, 4.3
			 2 = sunos 3.0, 4.0 
			 3 = vax v9 (bell labs), hpux or RISCos *)
    val pstruct : object ref
    val ratio : int ref
    val realarray0 : A.realarray
    val sighandler : ((int * int * unit cont) -> unit cont) ref
    val softmax : int ref
    (*val vector0 : 'a A.vector*)
    val vector0 : 'a vector
end
