(* ex-multicast.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A multi-cast channel abstraction.
 *)

(* BEGIN EXAMPLE *)
functor Multicast (BC : BUFFER_CHAN) : MULTICAST =
  struct
    structure CML = BC.CML

    open CML

    datatype 'a mchan = MChan of ('a request chan * 'a event chan)
	 and 'a request = Message of 'a | NewPort

    fun mChannel () = let
          val reqCh = channel() and respCh = channel()
          fun mkPort outFn = let
                val buf = BC.buffer()
		val inCh = channel()
		fun tee () = let val m = accept inCh
		      in
			BC.bufferSend(buf, m);
			outFn m;
			tee()
		      end
                in
		  spawn tee;
                  (fn m => send(inCh, m), BC.bufferReceive buf)
		end
          fun server outFn = let
                fun handleReq NewPort = let val (outFn', port) = mkPort outFn
                      in
                        send (respCh, port);
                        outFn'
                      end
                  | handleReq (Message m) = (outFn m; outFn)
                in
                  server (sync (wrap (receive reqCh, handleReq)))
                end
          in
            spawn (fn () => server (fn _ => ()));
            MChan(reqCh, respCh)
          end

    fun newPort (MChan(reqCh, respCh)) = (send (reqCh, NewPort); accept respCh)

    fun multicast (MChan(ch, _), m) = send (ch, Message m)

  end (* Multicast *)
(* END EXAMPLE *)
