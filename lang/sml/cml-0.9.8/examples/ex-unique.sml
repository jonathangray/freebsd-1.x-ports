(* ex-unique.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *)

(* BEGIN EXAMPLE *)
structure UniqueId : UNIQUE_ID =
  struct
    datatype id = ID of int

    val idCh : id CML.chan = CML.channel ()

    fun server i = (CML.send (idCh, ID i);  server (i+1))

    fun nextId () = CML.accept idCh

    val _ = RunCML.logChannel ("UniqueId.idCh", idCh)

    val _ = RunCML.logServer ("UniqueId",
              fn () => (CML.spawn (fn () => server 0); ()),
	      fn () => ())

  end (* structure UniqueId *)
(* END EXAMPLE *)
