(* access.sml
 *
 * COPYRIGHT (c) 1989 by AT&T Bell Laboratories
 *)

structure Access : ACCESS =
struct

  structure P = 
    struct
    (* Various primitive operations.  Those that are designated "inline" are
     * expanded into lambda representation in the InlineOps structure.
     *)
      datatype primop
	= IADD | ISUB | IMUL | IDIV | INEG	(* integer arithmetic *)
	| IGE | IGT | ILE | ILT			(* integer comparisons *)
	| LESSU | GEQU				(* unsigned comparisons *)
	| IEQL | INEQ				(* integer (and pointer) equality *)
	| FADDd | FSUBd | FMULd | FDIVd | FNEGd	(* double precision arithmetic *)
        | FABSd					(* double precision abs *)
	| FGEd | FGTd | FLEd | FLTd		(* double precision comparisons *)
	| FEQLd | FNEQd				(* double precision equality *)
	| POLYEQL | POLYNEQ			(* polymorphic equality *)
	| BOXED | UNBOXED			(* boxity tests *)
        | LENGTH				(* vector, string, array, ... length *)
        | OBJLENGTH				(* length of arbitrary heap object *)
	| CAST
	| GETHDLR | SETHDLR			(* get/set exn handler pointer *)
	| GETVAR | SETVAR			(* get/set var register *)
	| MAKEREF				(* allocate a ref cell *)
	| CALLCC | CAPTURE | THROW		(* continuation operations *)
	| STORE					(* bytearray update *)
	| INLSTORE				(* inline bytearray update *)
	| ORDOF					(* string subscript *)
	| INLORDOF				(* inline string subscript *)
	| INLBYTEOF				(* inline bytearray subscript *)
	| DEREF					(* dereferencing *)
	| ASSIGN				(* assignment; this is short for *)
						(* an update operation *)
	| UPDATE				(* array update (maybe boxed) *)
	| INLUPDATE				(* inline array update (maybe boxed) *)
	| BOXEDUPDATE				(* boxed array update *)
	| UNBOXEDUPDATE				(* unboxed array update *)
	| SUBSCRIPT				(* array subscript *)
	| INLSUBSCRIPT				(* inline array subscript *)
	| FLOOR					(* double precision -> int conversion *)
	| ROUND					(* UNIMPLEMENTED *)
	| REAL					(* int -> double precision conversion *)
	| SUBSCRIPTV				(* vector subscript *)
	| INLSUBSCRIPTV				(* inline vector subscript *)
	| FSUBSCRIPTd				(* real array subscript *)
	| INLFSUBSCRIPTd			(* inline real array subscript *)
	| FUPDATEd				(* real array update *)
	| INLFUPDATEd				(* inline real array update *)
	| RSHIFT | LSHIFT			(* logical shifts *)
	| ORB | ANDB | XORB | NOTB		(* bit-wise logical operations *)
	| GETTAG				(* extract the tag portion of an *)
						(* object's descriptor as an ML int *)
	| MKSPECIAL				(* make a special object *)
	| SETSPECIAL				(* set the state of a special object *)
	| GETSPECIAL				(* get the state of a special object *)
	| USELVAR | DEFLVAR

      fun pr_primop DEREF = "!"
        | pr_primop IMUL = "*" 
        | pr_primop IADD  = "+"
        | pr_primop ISUB = "-"
        | pr_primop ASSIGN = ":="
        | pr_primop ILT  = "<"
        | pr_primop ILE = "<="
        | pr_primop IGT  = ">"
        | pr_primop IGE = ">="
        | pr_primop LESSU = "lessu"
        | pr_primop GEQU = "gequ"
        | pr_primop BOXED = "boxed"
        | pr_primop UNBOXED = "unboxed"
        | pr_primop IDIV = "div"
        | pr_primop CAST = "cast"
        | pr_primop POLYEQL = "polyeql"
	| pr_primop POLYNEQ = "polyneq"  
        | pr_primop FADDd = "faddd"
        | pr_primop FDIVd = "fdivd"
        | pr_primop FEQLd = "feqld"
        | pr_primop FGEd  = "fged"
        | pr_primop FGTd  = "fgtd"
        | pr_primop FLEd = "fled"
        | pr_primop FLTd = "fltd"
        | pr_primop FMULd = "fmuld"
        | pr_primop FNEQd = "fneqd"
        | pr_primop FSUBd = "fsubd"
        | pr_primop FNEGd = "fnegd"
        | pr_primop FABSd = "fabsd"
        | pr_primop GETHDLR = "gethdlr"
        | pr_primop IEQL = "ieql"
        | pr_primop INEQ = "ineq"
        | pr_primop MAKEREF = "makeref"
        | pr_primop ORDOF = "ordof"
        | pr_primop SETHDLR = "sethdlr"
        | pr_primop LENGTH = "length"
        | pr_primop OBJLENGTH = "objlength"
        | pr_primop CALLCC = "callcc"
        | pr_primop CAPTURE = "capture"
        | pr_primop THROW = "throw"
        | pr_primop STORE = "store"
        | pr_primop SUBSCRIPT = "subscript"
        | pr_primop BOXEDUPDATE = "boxedupdate"
        | pr_primop UNBOXEDUPDATE = "unboxedupdate"
        | pr_primop UPDATE = "update"
        | pr_primop INEG = "~"
        | pr_primop INLSUBSCRIPT = "inlsubscript"
        | pr_primop INLSUBSCRIPTV = "inlsubscriptv"
        | pr_primop INLUPDATE = "inlupdate"
        | pr_primop INLBYTEOF = "inlbyteof"
        | pr_primop INLSTORE = "inlstore"
        | pr_primop INLORDOF = "inlordof"
        | pr_primop FLOOR = "floor"
        | pr_primop ROUND = "round"
        | pr_primop REAL = "real"
        | pr_primop FSUBSCRIPTd = "subscriptf"
        | pr_primop FUPDATEd = "updatef"
        | pr_primop INLFSUBSCRIPTd = "inlsubscriptf"
        | pr_primop INLFUPDATEd = "inlupdatef"
        | pr_primop SUBSCRIPTV = "subscriptv"
        | pr_primop RSHIFT = "rshift"
        | pr_primop LSHIFT = "lshift"
        | pr_primop ORB = "orb"
        | pr_primop ANDB = "andb"
        | pr_primop XORB = "xorb"
        | pr_primop NOTB = "notb"
        | pr_primop GETVAR = "getvar"
        | pr_primop SETVAR = "setvar"
	| pr_primop GETTAG = "GETTAG"
	| pr_primop MKSPECIAL = "MKSPECIAL"
	| pr_primop SETSPECIAL = "SETSPECIAL"
	| pr_primop GETSPECIAL = "GETSPECIAL"
        | pr_primop USELVAR = "uselvar"
        | pr_primop DEFLVAR = "deflvar"
    end

  type lvar = int      (* lambda variable id number *)
  type slot = int      (* position in structure record *)
  type path = int list (* slot chain terminated by lambda variable id number *)
  type primop = P.primop

  (* access: how to find the dynamic value corresponding to a variable.
    A PATH is an absolute address from a lambda-bound variable (i.e. we find
    the value of the lambda-bound variable, and then do selects from that).
    PATH's are kept in reverse order.   A SLOT is a position in a structure,
    and is relative to the address of the lambda-bound variable for the
    structure.   INLINE means that there is no dynamic value for the variable,
    which is a closed function: instead the compiler will generate "inline"
    code for the variable.  If we need a dynamic value, we must eta-expand
    the function.

    See modules.sig for the invariants of access paths in environments *)

  datatype access 
    = SLOT of slot
    | PATH of path  
    | INLINE of primop

  datatype conrep
      = UNTAGGED
      | TAGGED of int
      | TAGGEDREC of int * int
      | UNTAGGEDREC of int
      | CONSTANT of int
      | TRANSPARENT
      | REF
      | VARIABLE of access (* exception constructor *)
      | VARIABLEc of access (* exception constructor with no argument *)

(* the different kinds of records *)
  datatype record_kind
    = RK_VECTOR
    | RK_RECORD
    | RK_SPILL
    | RK_CLOSURE
    | RK_CONT

  (* local *)
    val varcount = ref 0
    exception NoLvarName
    val lvarNames : string Intmap.intmap = Intmap.new(32, NoLvarName)
    val name = Intmap.map lvarNames
    val giveLvarName = Intmap.add lvarNames

  val saveLvarNames = System.Control.saveLvarNames
  fun mkLvar () : lvar = (inc varcount; !varcount)
  fun sameName(v,w) =
      if !saveLvarNames
      then giveLvarName(v,name w)
	     handle NoLvarName => (giveLvarName(w, name v)
				      handle NoLvarName => ())
      else ()
  fun dupLvar v =
      (inc varcount;
       if !saveLvarNames
       then giveLvarName(!varcount,name v) handle NoLvarName => ()
       else ();
       !varcount)
  fun namedLvar(id: Symbol.symbol) =
      (inc varcount;
       if !saveLvarNames then giveLvarName(!varcount,Symbol.name id) else ();
       !varcount)
  fun lvarName(lv : lvar) : string =
      (name lv ^ makestring lv) handle NoLvarName => makestring lv

  fun pr_lvar(lvar:lvar) = makestring(lvar)
  fun pr_slot(slot:slot) = makestring(slot)
  fun pr_path'[] = "]"
    | pr_path'[x:int] = makestring x ^ "]"
    | pr_path'((x:int)::rest)= makestring x ^ "," ^ pr_path' rest
  fun pr_path path = "[" ^ pr_path' path
  fun pr_access (SLOT slot) = "SLOT(" ^ pr_slot slot ^ ")"
    | pr_access (PATH path) = "PATH(" ^ pr_path path ^ ")"
    | pr_access (INLINE po) = "INLINE(" ^ P.pr_primop po ^ ")"

end  (* structure Access *)
