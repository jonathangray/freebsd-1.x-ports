(* callcc.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Safe versions of callcc, etc., for multi-threaded applications.  We tag
 * continuations with the id of the thread in which they are a created.  A
 * thread may only throw to one of its own continuations, otherwise the
 * exception BadCont is raised.
 *)

signature CONCUR_CALLCC =
  sig
    exception BadCont
    type 'a cont
    val callcc : ('1a cont -> '1a) -> '1a
    val throw : 'a cont -> 'a -> 'b
  end

functor ConcurCallCC (CML : CONCUR_ML) : CONCUR_CALLCC =
  struct

    exception BadCont

    datatype 'a cont = CONT of (CML.thread_id * 'a General.cont)

    fun callcc f = General.callcc (fn k => f (CONT(CML.getTid(), k)))

    fun throw (CONT(pid, k)) = if (CML.sameThread (pid, CML.getTid()))
	  then (General.throw k)
	  else raise BadCont

  end (* SafeCallCC *)

