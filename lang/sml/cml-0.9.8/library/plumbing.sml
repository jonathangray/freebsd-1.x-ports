(* plumbing.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Miscellaneous plumbing fixtures (even the kitchen sink).
 *)

signature PLUMBING =
  sig
    structure CML : CONCUR_ML
    val sink : 'a CML.event -> unit
    val source : 'a -> 'a CML.event
    val iterate : ('1a * ('1a -> '1a)) -> '1a CML.event
    val connect : ('a CML.event * ('a -> unit CML.event)) -> unit
    val filter : ('a CML.event * ('a -> 'b) * ('b -> unit CML.event)) -> unit
  end (* PLUMBING *)

functor Plumbing (CML : CONCUR_ML) : PLUMBING =
  struct

    structure CML = CML

    local open CML in

  (* sink : 'a event -> unit *)
    fun sink evt = let
	  fun loop () = (sync evt; loop())
	  in
	    spawn loop;
	    ()
	  end

  (* source : 'a -> 'a event *)
    val source = always

  (* iterate : ('1a * ('1a -> '1a)) -> '1a event *)
    fun iterate (init, f) = let
	  val ch = channel()
	  fun loop x = (send(ch, x); loop(f x))
	  in
	    spawn (fn () => loop init);
	    receive ch
	  end

  (* connect : ('a event * ('a -> unit event)) -> unit *)
    fun connect (src, dst) = let
	  fun loop () = (sync (dst (sync src)); loop())
	  in
	    spawn loop;
	    ()
	  end

  (* filter : ('a event * ('a -> 'b) * ('b -> unit event)) -> unit *)
    fun filter (src, f, dst) = let
	  fun loop () = (sync (dst (f (sync src))); loop())
	  in
	    spawn loop;
	    ()
	  end

    end (* local open CML *)

  end (* functor Plumbing *)
