(* multicast.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *)

functor Multicast (BC : BUFFER_CHAN) : MULTICAST =
  struct
    structure CML = BC.CML

    open CML

    datatype 'a mchan = MChan of ('a request chan * 'a event chan)
	 and 'a request = Message of 'a | NewPort of thread_id
	 and 'a message = Msg of 'a | NewInput of 'a message chan

    fun mChannel () = let
          val reqCh = channel() and respCh = channel()
          fun mkPort (outCh, tid) = let
                val buf = BC.buffer()
		val inCh = channel()
		fun tee inCh = sync (choose [
			wrap (receive inCh,
			  fn (Msg m) => (
			      BC.bufferSend(buf, m);
			      send(outCh, Msg m);
			      tee inCh)
			   | (NewInput newCh) => tee newCh),
			wrap (threadWait tid,
			  fn () => send(outCh, NewInput inCh))
		      ])
                in
		  spawn (fn () => tee inCh);
                  (inCh, BC.bufferReceive buf)
		end
	  fun sink () = let
		val ch = channel()
		fun loop inCh = (case (accept inCh)
		     of (Msg _) => loop inCh
		      | (NewInput ch) => loop ch)
		in
		  spawn (fn () => loop ch);
		  ch
		end
          fun server outCh = let
                fun handleReq (NewPort tid) = let
		      val (outCh', port) = mkPort(outCh, tid)
                      in
                        send (respCh, port);
                        outCh'
                      end
                  | handleReq (Message m) = (send(outCh, Msg m); outCh)
                in
                  server (sync (wrap (receive reqCh, handleReq)))
                end
          in
            spawn (fn () => server(sink()));
            MChan(reqCh, respCh)
          end

    fun newPort (MChan(reqCh, respCh)) = (
	  send (reqCh, NewPort (getTid()));
	  accept respCh)

    fun multicast (MChan(ch, _), m) = send (ch, Message m)

  end (* Multicast *)
