(* Copyright 1989 by AT&T Bell Laboratories *)
(* primop.sig *)

signature PRIMOP =
  sig

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
      | BOXED | UNBOXED				(* boxity tests *)
      | LENGTH					(* vector, string, array, ... length *)
      | OBJLENGTH				(* length of arbitrary heap object *)
      | CAST
      | GETHDLR | SETHDLR			(* get/set exn handler pointer *)
      | GETVAR | SETVAR				(* get/set var register *)
      | MAKEREF					(* allocate a ref cell *)
      | CALLCC | CAPTURE | THROW		(* continuation operations *)
      | STORE					(* bytearray update *)
      | INLSTORE				(* inline bytearray update *)
      | ORDOF					(* string subscript *)
      | INLORDOF				(* inline string subscript *)
      | INLBYTEOF				(* inline bytearray subscript *)
      | DEREF					(* dereferencing *)
      | ASSIGN					(* assignment; this is short for *)
						(* an update operation *)
      | UPDATE					(* array update (maybe boxed) *)
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
      | INLFSUBSCRIPTd				(* inline real array subscript *)
      | FUPDATEd				(* real array update *)
      | INLFUPDATEd				(* inline real array update *)
      | RSHIFT | LSHIFT				(* logical shifts *)
      | ORB | ANDB | XORB | NOTB		(* bit-wise logical operations *)
      | GETTAG					(* extract the tag portion of an *)
						(* object's descriptor as an ML int *)
      | MKSPECIAL				(* make a special object *)
      | SETSPECIAL				(* set the state of a special object *)
      | GETSPECIAL				(* get the state of a special object *)
      | USELVAR | DEFLVAR

    val pr_primop: primop -> string

  end (* PRIMOP *)
