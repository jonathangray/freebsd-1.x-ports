(* xio.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This code implements the low-level I/O of the X-protocol.
 *
 * NOTE: the implementation of close doesn't really work, since the socket may
 * end up being closed before the output buffer is actually flushed (race condition).
 *)

signature XIO =
  sig

    exception LostReply
    exception ErrorReply of XErrors.xerror

    type connection

    val openConn : int -> connection
    val closeConn : connection -> unit

    val sameConn : (connection * connection) -> bool

    val request : connection -> string -> unit
    val requestAndChk : connection -> string -> unit CML.event
    val requestReply : connection -> string -> string CML.event
    val requestMultiReply : connection -> (string * (string -> int))
	  -> string CML.event
    val requestWithExposures : connection
	  -> (string * (unit -> Geometry.rect list) CML.cond_var) -> unit

    val flushOut : connection -> unit

    val waitForXEvent : connection -> XEventTypes.xevent CML.event

    val readXError : connection -> (int * string)

  end

structure XIo : XIO =
  struct

    exception LostReply
    exception ErrorReply of XErrors.xerror

    local
      structure BA = ByteArray
      open CML System.Unsafe.SysIO

      val ba2str : bytearray -> string = System.Unsafe.cast
      val implode2ba : string list -> bytearray = System.Unsafe.cast implode

      val << = Bits.lshift
      val ++ = Bits.orb
      infix << ++

      val newBuf = System.Unsafe.Assembly.A.create_b
      val bufsz = 2048

    (* time to wait before flushing a non-empty output buffer *)
      val flushTimeOut = timeout(TIME{sec=0, usec=250000})
    in

  (* request messages sent to the sequencer by clients *)
    datatype req_msg
      = RequestFlush
      | RequestQuit
      | Request of string
      | RequestAndChk of (string * reply chan)
      | RequestReply of (string * reply chan)
      | RequestReplies of (string * reply chan * (string -> int))
      | RequestExposures of (string * (unit -> Geometry.rect list) cond_var)

  (* replies from the sequencer to client requests *)
    and reply
      = ReplyLost		    (* The reply was lost somewhere in transit *)
      | Reply of string		    (* A normal reply *)
      | ReplyError of string	    (* The server returned an error message *)

  (* messages from the sequencer to the output buffer *)
    datatype out_msg
      = OutFlush
      | OutQuit
      | OutMsg of string


  (** The input stream manager **
   * This monitors the input stream from the X-server, and breaks it up into
   * individual messages, which are sent on outCh to be unmarshalled and routed
   * by the sequencer.  Each message to the sequencer consists is a triple of
   * the message code, sequence number and message string.
   *)
    fun inbuf (outCh, fd) () = let
	  datatype frag
	    = NoFrag | HdrFrag of string | Frag of (int * int * int * string)
	  val buf = newBuf bufsz
	  fun getMsgHdr (buf, i) = let
		val seqn = ((BA.sub(buf, i+2)) << 8) ++ (BA.sub(buf, i+3))
		in
		  case BA.sub(buf, i)
		   of 1 => {
			  code = 1,  (* reply *)
			  seqn = seqn,
			  len = (32
			      + ((BA.sub(buf, i+4))<<26) + ((BA.sub(buf, i+5))<<18)
			      + ((BA.sub(buf, i+6))<<10) + ((BA.sub(buf, i+7))<<2))
			}
		    | k => {code = k, seqn = seqn, len = 32}
		end
(* +DEBUG *)
val getMsgHdr = fn arg => let
      val (res as {code, seqn, len}) = getMsgHdr arg
      in
	XDebug.trace (XDebug.ioTM, fn () => [
	    "getMsgHdr: code = ", makestring code, ", seqn = ", makestring seqn,
	    ", len = ", makestring len, "\n"
	  ]);
	res
      end
(* -DEBUG *)
	  fun sendMsg arg = send (outCh, arg)
	(* read input, split into messages and send them on outCh *)
	  fun readAndSend (Frag(code, seqn, k, s)) = let
		fun get buf = let
		      fun g i = if (i < k) then g(i+readi(fd, buf, i, k-i)) else ()
		      in
			g 0
		      end
		val rest = if (k > bufsz)
		      then let
			val tmpBuf = newBuf k
			in
			  get tmpBuf;
			  ba2str tmpBuf
			end
		      else (get buf; BA.extract(buf, 0, k))
		val str = s ^ rest
		in
		  sendMsg (code, seqn, str);
		  NoFrag
		end
	    | readAndSend frag = let
		val nb = (case frag
		     of NoFrag => read(fd, buf, bufsz)
		      | (HdrFrag s) => let
			  val l = size s
			  fun cpy i = (BA.update(buf, i, ordof(s, i)); cpy(i+1))
			  in
			    (cpy 0) handle _ => ();
			    readi(fd, buf, l, bufsz-l) + l
			  end)
		fun split i = if (i+8 <= nb)
		      then let
			val {code, seqn, len} = getMsgHdr (buf, i)
			val next = i+len
			in
			  if (next <= nb)
			    then (
			      sendMsg(code, seqn, BA.extract(buf, i, len));
			      split next)
			    else Frag(code, seqn, next-nb, BA.extract(buf, i, nb-i))
			end
		      else if (i < nb)
			then HdrFrag(BA.extract(buf, i, nb-i))
		        else NoFrag
		in
		  split 0
		end
	  val inEvt = syncOnInput fd
	  fun syncRead frag = (sync inEvt; readAndSend frag) handle _ => exit()
	  fun loop frag = loop (syncRead frag)
	  in
	    loop NoFrag
	  end (* inbuf *)


  (** The output stream manager. **)
    fun outbuf (inCh, fd) () = let
	  fun quit () = (closef fd; exit())
	  fun flushBuf strs = let
		val s = implode2ba(rev strs)
		in
(* +DEBUG *)
XDebug.trace (XDebug.ioTM, fn () => [
    "Flush: ", makestring (List.length strs), " msgs, ",
    makestring(BA.length s), " bytes\n"
  ]);
(* -DEBUG *)
		  write(fd, s, BA.length s)
		end
	  fun insert (s, (strs, nb)) = let
		val n = size s
		in
		  if (n+nb > bufsz)
		    then (flushBuf strs; ([s], n))
		    else (s::strs, n+nb)
		end
	  fun loop ([], _) = (case (accept inCh)
	       of OutFlush => loop([], 0)
		| (OutMsg s) => loop([s], size s)
		| OutQuit => quit())
	    | loop (buf as (strs, _)) = CML.select [
		  wrap(flushTimeOut, fn _ => (flushBuf strs; loop([], 0))),
		  wrap(receive inCh,
		    fn OutFlush => (flushBuf strs; loop([], 0))
		     | (OutMsg s) => loop(insert(s, buf))
		     | OutQuit => (flushBuf strs; quit()))
		]
	  in
	    loop ([], 0)
	  end (* outbuf *)


  (** The sequencer **
   * The sequencer is responsible for matching replies with requests. All requests to
   * the X-server go through the sequencer, as do all messages from the X-server.
   * The sequencer communicates on five fixed channels:
   *   reqCh  -- request messages from clients
   *   inCh   -- reply, error and event messages from the server (via the input buffer)
   *   outCh  -- requests messages to the output buffer
   *   xevtCh -- X-events to the X-event buffer
   *   errCh  -- errors to the error handler
   * In addition, the sequencer sends replies to clients on the reply channel that was
   * bundled with the request.
   *)
    local
    (* the kind of reply that is pending for an outstanding request in the
     * outstanding request queue
     *)
      datatype outstanding_reply
	= ErrorChk of (int * reply chan)
	| OneReply of (int * reply chan)
	| MultiReply of (int * reply chan * (string -> int) * string list)
	| ExposureReply of (int * (unit -> Geometry.rect list) cond_var)

(* +DEBUG *)
fun dumpPendingQ (seqn : int, ([], [])) = XDebug.errTrace (fn () => [
	"PendingQ(", makestring seqn, "): empty\n"
      ])
  | dumpPendingQ (seqn, (front, rear)) = let
      fun item2s (ErrorChk(n, _)) = "  ErrorChk #" ^ (makestring n) ^ "\n"
	| item2s (OneReply(n, _)) = "  OneReply #" ^ (makestring n) ^ "\n"
	| item2s (MultiReply(n, _, _, _)) = "  MultiReply #" ^ (makestring n) ^ "\n"
	| item2s (ExposureReply(n, _)) = "  ExposureReply #" ^ (makestring n) ^ "\n"
      fun dump ([], l) = (rev l)
	| dump (x::r, l) = dump(r, (item2s x) :: l)
      in
	XDebug.errTrace (fn () =>
	    "PendingQ(" :: (makestring seqn) :: "):\n" :: (dump(front @ (rev rear), []))
	  )
      end
(* -DEBUG *)

      fun seqnOf (ErrorChk(seqn, _)) = seqn
	| seqnOf (OneReply(seqn, _)) = seqn
	| seqnOf (MultiReply(seqn, _, _, _)) = seqn
	| seqnOf (ExposureReply(seqn, _)) = seqn

      fun sendReply arg = (spawn(fn () => send arg); ())

      fun sendReplies (ch, replies) = let
	    fun loop [] = () | loop (s::r) = (send(ch, Reply s); loop r)
	    in
	      spawn (fn () => loop(rev replies)); ()
	    end

      fun insert (x, (front, rear)) = (front, x::rear)

    (* Synchronize the queue of outstanding requests with the sequence number n.  Return
     * the pair (f, q), where q is the synchronized queue and f is true if the head
     * of q has sequence number b.
     *)
      fun syncOutstandingQ (n, q) = let
	    fun discardReply (ErrorChk(_, ch)) = sendReply(ch, Reply "")
	      | discardReply (OneReply(_, ch)) = sendReply(ch, ReplyLost)
	      | discardReply (MultiReply(_, ch, _, [])) = sendReply(ch, ReplyLost)
	      | discardReply (MultiReply(_, ch, _, replies)) = sendReplies(ch, replies)
	      | discardReply (ExposureReply(_, syncV)) =
		  writeVar (syncV, fn () => raise LostReply)
	    fun scan (q' as ([], [])) = (false, q')
	      | scan ([], rear) = scan (rev rear, [])
	      | scan (q' as ((rpend :: r), rear)) = let
		  val seqn = seqnOf rpend
		  in
		    if (seqn < n)
		      then (discardReply rpend; scan (r, rear))
		      else if (seqn > n)
			then (false, q')
			else (true, q')
		  end
	    in
	      scan q
	    end

    (* extract the outstanding request corresponding to the given reply message (with
     * sequence number n).  If all of the expected replies have been received,
     * then send the extracted reply to the requesting client.
     *)
      fun extractReply (n, reply, q) = (
	    case (syncOutstandingQ(n, q))
	     of (true, (OneReply(_, ch)::r, rear)) => (
		  sendReply(ch, Reply reply); (r, rear))
	      | (true, (MultiReply(seqn, ch, remain, replies)::r, rear)) => (
		  if ((remain reply) = 0)
		    then (sendReplies(ch, reply::replies); (r, rear))
		    else (MultiReply(seqn, ch, remain, reply::replies)::r, rear))
	      | _ => 
(* DEBUG *) (dumpPendingQ(n, q);
MLXError.impossible "[XIo.extractReply: bogus pending reply queue]"
(* DEBUG *) )
	    (* end case *))

    (* extract the outstanding request corresponding to the given exposure message
     * (with seqence number n).
     *)
      fun extractExpose (n, reply, q) = (
	    case (syncOutstandingQ(n, q))
	     of (true, (ExposureReply(_, syncV)::r, rear)) => (
		  writeVar (syncV, fn () => reply); (r, rear))
(* for now, just drop it.  When the gc-server supports graphics-exposures, these
 * shouldn't happen. *)
	      | _ => q
(* +DEBUG 
(dumpPendingQ(n, q);
MLXError.impossible "[XIo.extractExpose: bogus pending reply queue]")
-DEBUG *)
	    (* end case *))

    (* extract the outstanding request corresponding to the given error message
     * (with seqence number n).
     *)
      fun extractErr (n, err, q) = (
	    case (syncOutstandingQ(n, q))
	     of (true, (ErrorChk(_, ch)::r, rear)) => (
		  sendReply(ch, ReplyError err); (r, rear))
	      | (true, (OneReply(_, ch)::r, rear)) => (
		  sendReply(ch, ReplyError err); (r, rear))
	      | (true, (MultiReply(_, ch, _, _)::r, rear)) => (
		  sendReply(ch, ReplyError err); (r, rear))
	      | (true, (ExposureReply(_, syncV)::r, rear)) => (
		  writeVar (syncV, fn () => raise ErrorReply(XReply.decodeError err));
		  (r, rear))
	      | (false, q') => q'
	      | _ =>
(* DEBUG *) (dumpPendingQ(n, q);
MLXError.impossible "[XIo.extractErr: bogus pending reply queue]"
(* DEBUG *) )
	    (* end case *))

      fun syncWithXEvt (n, q) = (
	    case (syncOutstandingQ(n, q))
	     of (true, (ErrorChk(_, ch)::r, rear)) => (
		  sendReply(ch, Reply ""); (r, rear))
	      | (_, q) => q
	    (* end case *))
    in
    fun sequencer (reqEvt, inCh, outCh, xevtCh, errCh) () = let
	  fun quit () = (send(outCh, OutQuit); exit())
	  val inEvt = receive inCh
	  fun doRequest (req, (lastOut, pending)) = (
		send(outCh, OutMsg req);
		(lastOut+1, pending))
	  fun doRequestAndChk ((req, replyCh), (lastOut, pending)) = let
		val n = lastOut+1
		in
		  send(outCh, OutMsg req);
		  (n, insert(ErrorChk(n, replyCh), pending))
		end
	  fun doRequestReply ((req, replyCh), (lastOut, pending)) = let
		val n = lastOut+1
		in
		  send(outCh, OutMsg req);
		  (n, insert(OneReply(n, replyCh), pending))
		end
	  fun doRequestReplies ((req, replyCh, remain), (lastOut, pending)) = let
		val n = lastOut+1
		in
		  send(outCh, OutMsg req);
		  (n, insert(MultiReply(n, replyCh, remain, []), pending))
		end
	  fun doRequestExposures ((req, syncV), (lastOut, pending)) = let
		val n = lastOut+1
		in
		  send(outCh, OutMsg req);
		  (n, insert(ExposureReply(n, syncV), pending))
		end
	(* gobble requests w/o blocking and then flush the buffer *)
	  fun gobbleAndFlush arg = let
		fun loop arg = (case (poll reqEvt)
		       of NONE => arg
			| (SOME RequestFlush) => loop arg
			| (SOME RequestQuit) => quit()
			| (SOME(Request req)) => loop (doRequest(req, arg))
			| (SOME(RequestAndChk req)) => loop (doRequestAndChk (req, arg))
			| (SOME(RequestReply req)) => loop (doRequestReply (req, arg))
			| (SOME(RequestReplies req)) =>
			    loop (doRequestReplies (req, arg))
			| (SOME(RequestExposures req)) =>
			    loop (doRequestExposures (req, arg))
		      (* end case *))
		val res = loop arg
		in
		  send(outCh, OutFlush);
		  res
		end
	  fun loop (arg as (lastReqOut, pending)) = let
	      (* handle a request from a client *)
		fun reqWrap RequestFlush = gobbleAndFlush arg
		  | reqWrap RequestQuit = quit()
		  | reqWrap (Request req) = (
		      send(outCh, OutMsg req);
		      (lastReqOut+1, pending))
		  | reqWrap (RequestAndChk req) =
		      gobbleAndFlush (doRequestAndChk (req, arg))
		  | reqWrap (RequestReply req) =
		      gobbleAndFlush (doRequestReply (req, arg))
		  | reqWrap (RequestReplies req) =
		      gobbleAndFlush (doRequestReplies (req, arg))
		  | reqWrap (RequestExposures req) =
		      gobbleAndFlush (doRequestExposures (req, arg))
	      (* handle a server-message (from the input buffer) *)
		fun inWrap (0, seqn, s) = (
		      send(errCh, (seqn, s));
		      (lastReqOut, extractErr(seqn, s, pending)))
		  | inWrap (1, seqn, s) =
		      (lastReqOut, extractReply(seqn, s, pending))
		  | inWrap (13, seqn, s) = let
		      open XEventTypes
		      fun pack (rl, GraphicsExposeXEvt{rect, count=0, ...}) = rect::rl
			| pack (rl, GraphicsExposeXEvt{rect, ...}) = (case (accept inCh)
			   of (13, _, s) =>
				pack (rect::rl, XReply.decodeGraphicsExpose s)
			    | _ => (
				MLXError.warning
				  "[XIo.sequencer: misleading GraphicsExpose count]";
				rect::rl))
		      val rects = pack ([], XReply.decodeGraphicsExpose s)
		      in
			(lastReqOut, extractExpose(seqn, rects, pending))
		      end
		  | inWrap (14, seqn, s) = (* NoExpose *)
		      (lastReqOut, extractExpose(seqn, [], pending))
		  | inWrap (k, seqn, s) = (
		      send (xevtCh, (k, s));
		      (lastReqOut, syncWithXEvt(seqn, pending)))
		in
		  loop (
		    CML.select [
			wrap (reqEvt, reqWrap),
			wrap (inEvt, inWrap)
		      ])
		end (* loop *)
	  in
	    loop (0, ([], []))
	  end (* sequencer *)
    end (* local *)


  (** The X-event buffer **
   *
   * The X-event buffer decodes and buffers X-events.  This thread also packs
   * expose events.  It communicates on two channels as follows:
   *   xevtMsgCh  --  raw messages from the sequencer
   *   xevtCh     --  decoded events to the window registry
   *)
    fun xeventBuffer (xevtMsgCh, xevtCh) = let
	  open XEventTypes
	  fun decode (k, s) = #2(XReply.decodeXEvent (k, s))
	  fun packExposeEvts (e as ExposeXEvt{window, ...}) = let
		fun pack (rl, ExposeXEvt{rects, count=0, ...}) = rects@rl
		  | pack (rl, ExposeXEvt{rects, ...}) =
		      pack (rects@rl, decode(accept xevtMsgCh))
		  | pack (rl, _) = (
		      MLXError.warning "[XIo.sequencer: misleading Expose count]";
		      rl)
		in
		  ExposeXEvt{window = window, rects = pack([], e), count = 0}
		end
	  fun doXEvent (msg, q) = (case (decode msg)
	       of (e as ExposeXEvt _) => (packExposeEvts e) :: q
		| e => (e :: q))
	  val getXEvt = receive xevtMsgCh
	  fun routeP () = let
		fun loop ([], []) = loop(doXEvent(accept xevtMsgCh, []), [])
		  | loop ([], rear) = loop(rev rear, [])
		  | loop (front as (x::r), rear) =
		      loop (CML.select [
			  wrap (getXEvt, fn msg => (front, doXEvent(msg, rear))),
			  wrap (transmit(xevtCh, x), fn () => (r, rear))
			])
		in
		  loop ([], [])
		end
	  in
	    routeP
	  end (* xeventBuffer *)


  (** The connection **)

    datatype connection = CONN of {
	  conn_id : unit ref,
	  xevt_ch : XEventTypes.xevent chan,
	  req_ch : req_msg chan,
	  err_ch : (int * string) chan,
	  flush : unit -> unit,
	  close : unit -> unit
	}

  (* Create the threads and internal channels to manage a connection to the
   * X server.  We assume that the connection request/reply has already been
   * dealt with.
   *)
    fun openConn fd = let
	  val inStrm = channel() and outStrm = channel()
	  val xevtStrm = channel() and xevtMsgStrm = channel()
	  val reqStrm = channel() and errStrm = channel()
	  val exposeStrm = channel()
	  fun flushFn () = send (reqStrm, RequestFlush)
	  fun closeFn () = (flushFn(); send(reqStrm, RequestQuit))
	  in
(******
	    spawn (sequencer (receive reqStrm, inStrm, outStrm, xevtMsgStrm, errStrm));
	    spawn (inbuf (inStrm, fd));
	    spawn (outbuf (outStrm, fd));
	    spawn (xeventBuffer (xevtMsgStrm, xevtStrm));
******)
(* DEBUG *) XDebug.xspawn ("Sequencer", sequencer (
		receive reqStrm, inStrm, outStrm, xevtMsgStrm, errStrm));
(* DEBUG *) XDebug.xspawn ("Inbuf", inbuf (inStrm, fd));
(* DEBUG *) XDebug.xspawn ("Outbuf", outbuf (outStrm, fd));
(* DEBUG *) XDebug.xspawn ("XEventBuffer", xeventBuffer (xevtMsgStrm, xevtStrm));
	    CONN{
		conn_id = ref (),
		xevt_ch = xevtStrm,
		req_ch = reqStrm,
		err_ch = errStrm,
		flush = flushFn,
		close = closeFn
	      }
	  end

    fun closeConn (CONN{close, ...}) = close()

    fun sameConn (CONN{conn_id=a, ...}, CONN{conn_id=b, ...}) = (a = b)

    fun request (CONN{req_ch, ...}) s = (send(req_ch, Request s))

    fun replyWrapper ReplyLost = raise LostReply
      | replyWrapper (ReplyError s) = raise ErrorReply(XReply.decodeError s)
      | replyWrapper (Reply s) = s

(** NOTE: these should be done using a guard event eventually *)
  (* Generate a request to the server and check on its successful completion. *)
    fun requestAndChk (CONN{req_ch, ...}) s = let
	  val replyCh1 = channel() and replyCh2 = channel()
	  in
	    send (req_ch, RequestAndChk(s, replyCh1));
	    send (req_ch, RequestReply(XRequest.requestGetInputFocus, replyCh2));
	    wrap (receive replyCh1,
	      fn (ReplyError s) => raise ErrorReply(XReply.decodeError s)
	       | _ => ())
	  end

    fun requestReply (CONN{req_ch, ...}) s = let
	  val replyCh = channel()
	  in
	    send (req_ch, RequestReply(s, replyCh));
	    wrap (receive replyCh, replyWrapper)
	  end

    fun requestMultiReply (CONN{req_ch, ...}) (s, remain) = let
	  val replyCh = channel()
	  in
	    send (req_ch, RequestReplies(s, replyCh, remain));
	    wrap (receive replyCh, replyWrapper)
	  end

    fun requestWithExposures (CONN{req_ch, ...}) (s, syncV) = let
	  val replyCh = channel()
	  in
	    send (req_ch, RequestExposures(s, syncV))
	  end

    fun flushOut (CONN{flush, ...}) = flush()

    fun waitForXEvent (CONN{xevt_ch, ...}) = receive xevt_ch
    fun readXError (CONN{err_ch, ...}) = accept err_ch

    end (* local *)
  end (* XIo *)

