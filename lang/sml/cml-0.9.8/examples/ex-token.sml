(* ex-token.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A simple token server.
 *)

(* BEGIN EXAMPLE *)
structure TokenServer : TOKEN_SERVER =
  struct
    structure CML = CML
    open CML

    datatype ('a, 'b) token = TOKEN of {
	operation : 'a -> 'b,		(* the protected operation *)
	acquire_ch : thread_id chan,	(* the channel for requesting the token *)
	check : unit -> unit,		(* check for token possession *)
	release : unit -> unit		(* release the token *)
      }

    exception NotTokenHolder
    fun newToken operFn = let
	  val acqCh = channel() and relCh = channel() and holdCh = channel()
	  fun server () = let
		val acquireEvt = receive acqCh
		val releaseEvt = receive relCh
		val myId = getTid()
		fun heldLoop curHolder = select [
			wrap (choose [releaseEvt, threadWait curHolder],
			  fn () => availLoop ()),
			wrap (transmit(holdCh, curHolder),
			  fn () => heldLoop curHolder)
		      ]
		and availLoop () = select [
			wrap (acquireEvt, fn id => heldLoop id),
			wrap (transmit(holdCh, myId), fn () => availLoop())
		      ]
		in
		  availLoop ()
		end
	  fun checkFn () = if sameThread(getTid(), accept holdCh)
		  then () else raise NotTokenHolder
	  in
	    spawn server;
	    TOKEN{
		operation = fn x => (checkFn(); operFn x),
		acquire_ch = acqCh,
		check = checkFn,
		release = fn () => send(relCh, ())
	      }
	  end

    fun getOperation (TOKEN{check, operation, ...}) = (check(); operation)
    fun releaseToken (TOKEN{check, release, ...}) = (check(); release())
    fun acquireToken (TOKEN{acquire_ch, ...}) = transmit(acquire_ch, getTid())

  end (* TokenServer *)
(* END EXAMPLE *)
