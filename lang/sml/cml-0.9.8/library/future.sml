(* future.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This is an implementation of a multi-lisp style future operation.
 *)

signature FUTURE =
  sig
    structure CML : CONCUR_ML
    val future : ('a -> '2b) -> 'a -> '2b CML.event
  end (* FUTURE *)

functor Future (CML : CONCUR_ML) =
  struct

    structure CML = CML

  (* future : ('a -> '2b) -> 'a -> '2b event
   * Spawn a thread to evaluate f applied to x and return an event that will
   * supply the result.  If f raises an exception, then synchronizing on the
   * future event will raise the same exception.
   *)
    fun future f x = let
	  datatype 'a msg_t = RESULT of 'a | EXN of exn
	  val resV = CML.condVar()
	  fun doit () = CML.writeVar (resV, (RESULT(f x) handle ex => EXN ex))
	  in
	    CML.spawn doit;
	    CML.wrap (
	      CML.readVarEvt resV,
	      fn (RESULT x) => x | (EXN ex) => raise ex)
	  end

  end (* functor Future *)
