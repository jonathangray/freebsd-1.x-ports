(* ex-buffer.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This is an implementation unbounded buffered channels.  Send operations never
 * block, but accept/receive operations may.
 *)

(* BEGIN EXAMPLE *)
functor BufferChan (CML : CONCUR_ML) : BUFFER_CHAN =
  struct
    structure CML = CML

    open CML

    datatype 'a buffer_chan = BC of {inch : 'a chan, outch : 'a chan}

    fun buffer () = let
	  val inCh = channel() and outCh = channel()
	  fun loop ([], []) = loop([accept inCh], [])
	    | loop (front as (x::r), rear) = select [
		  wrap (receive inCh, fn y => loop(front, y::rear)),
		  wrap (transmit(outCh, x), fn () => loop(r, rear))
		]
	    | loop ([], rear) = loop(List.rev rear, [])
	  in
	    spawn (fn () => loop([], []));
	    BC{inch=inCh, outch=outCh}
	  end

    fun bufferSend (BC{inch, ...}, x) = send(inch, x)
    fun bufferAccept (BC{outch, ...}) = accept outch
    fun bufferReceive (BC{outch, ...}) = receive outch

  end (* functor BufferChan *)
(* END EXAMPLE *)
