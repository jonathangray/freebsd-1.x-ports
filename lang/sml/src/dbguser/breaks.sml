(* Primitive breakpoint support. *)

structure UserDebugBreaks = struct
local open UserDebugUtil UserDebugInterface
in
  (* Foundations *)
  datatype break = TIME of time
                 | EVENT of place

  val breakList = ref (nil: (int*break) list)
  val nextBreak = ref 1

  fun insertBreak (b:break): int  =
    (breakList := ((!nextBreak,b)::(!breakList));
     !nextBreak before (inc nextBreak))
  
  fun removeBreak (bn:int) : bool  = 
    let 
      exception NoSuchBreak
      fun del ((n,b)::r) = if n = bn then r else ((n,b)::(del r))
	| del (nil) = raise NoSuchBreak
    in
      (breakList := del(!breakList);
       true) 
       handle NoSuchBreak => false
    end
  
 (* Utilities. *)
 fun getBreak n =
 (* Return the breakpoint with given id, if any. *)
     onSome(#2, select (fn (k, _) => k = n) (!breakList))

 fun breakId ev =
 (* Return the id of some breakpoint at the given event. *)
     let fun eqb (_, EVENT e) = (e = ev)
	   | eqb _ = false
     in onSome (#1, select eqb (!breakList))
     end

 fun breakIdAtTime tm =
 (* Return the id of some breakpoint at the given time. *)
     let fun eqb (_, TIME t) = (t = tm)
	   | eqb _ = false
     in onSome (#1, select eqb (!breakList))
     end

 (* Each function on the breakFuncList will be invoked whenever the
  * corresponding breakpoint is reached.
  * Typically f would be a function to show a variable's value, or to
  * continue (e.g. by calling forward()) if a certain value holds.
  * Using this technique breakpoints can be made conditional. *)
 val breakFuncList = ref ([] : (int * (unit -> unit)) list)

 fun getBreakFunc bn =
 (* Get the break function at the given breakpoint. *)
     lookup (!breakFuncList) bn

 fun resetBreakFunc bn =
 (* Remove the break function from the given breakpoint.*)
     breakFuncList := choose (fn (n, _) => n <> bn) (!breakFuncList)

 (* Note: commands and functions that may affect the emacs display are
    in dbguser/commands.sml *)

end (* local *)
end (* structure *)
