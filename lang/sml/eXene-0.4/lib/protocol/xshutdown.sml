(* xshutdown.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This is the shutdown server for eXene.  Log open connections and close them on
 * shutdown.
 *)

structure XShutdown =
  struct
    local
      open CML

      datatype req
	= LogConn of XIo.connection
	| UnlogConn of XIo.connection
	| Shutdown

      val reqCh : req chan = channel()
      val replyCh : unit chan = channel()

      fun startServer () = let
	    fun loop conns = (case (accept reqCh)
		 of LogConn arg => loop (arg::conns)
		  | UnlogConn conn => let
		      fun remove [] = []
			| remove (c :: r) =
			    if XIo.sameConn(c, conn) then r else (c :: (remove r))
		      in
			loop (remove conns)
		      end
		  | Shutdown => (
		      app XIo.closeConn conns;
		      send(replyCh, ())))
	    in
	      spawn (fn () => loop []); ()
	    end

      fun shutdown () = (send(reqCh, Shutdown); accept replyCh)

      val _ = RunCML.logChannel("eXene-shutdown:reqCh", reqCh)
      val _ = RunCML.logChannel("eXene-shutdown:replyCh", replyCh)
      val _ = RunCML.logServer("eXene-shutdown", startServer, shutdown)

    in

    fun logConnection arg = send(reqCh, LogConn arg)
    fun unlogConnection fd = send(reqCh, UnlogConn fd)

    end (* local *)
  end (* XShutdown *)
