(* cobegin
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *)

signature COBEGIN = sig
    structure CML : CONCUR_ML
    val cobegin : (unit -> unit) list -> unit CML.event
  end (* COBEGIN *)

functor Cobegin (CML : CONCUR_ML) : COBEGIN =
  struct
    structure CML : CONCUR_ML = CML

    fun cobegin fl = let
	  fun spawn ([], l) = l
	    | spawn (p::r, l) = let
		val id = CML.spawn p
		in
		  spawn(r, (id, CML.wrap (CML.threadWait id, fn () => id))::l)
		end
	  val extract = map (fn (_, evt) => evt)
	  fun barrier [] = ()
	    | barrier l = let
		val id = CML.select (extract l)
		fun f ((x as (id', evt)) :: r) =
		      if CML.sameThread(id, id') then r else (x :: (f r))
		in
		  barrier (f l)
		end
	  in
	    CML.threadWait (CML.spawn (fn () => barrier(spawn(fl, []))))
	  end

  end (* functor Cobegin *)
