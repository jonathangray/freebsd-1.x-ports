(* cio.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This functor implements concurrent versions of most of the SML/NJ IO
 * operations.
 *)

functor ConcurIO (RunCML : RUN_CML) : CONCUR_IO =
  struct
    structure CML = RunCML.CML

    exception Io = IO.Io

    fun error (cmd, name, msg) = raise Io(implode[cmd, " \"", name, "\": ", msg])

    structure SysIO = System.Unsafe.SysIO
    exception SystemCall = System.Unsafe.CInterface.SystemCall
    val closef = System.Unsafe.CInterface.wrap_sysfn "close" SysIO.closef
    val stringToBytearray : string -> ByteArray.bytearray = System.Unsafe.cast

    open CML

  (* A stream identifier is an int ref.  For streams that have a file descriptor
   * the ref contains the file descriptor.
   *)
    type stream_id_t = int ref

  (* A thread to keep track of the open streams (other than the standard ones). *)
    local
      type strm_info_t = {id : stream_id_t, kill : unit -> unit}
      datatype msg_t
	= OPEN_STRM of strm_info_t
	| REMOVE of stream_id_t
	| KILL_ALL of unit cond_var
      val (reqCh : msg_t chan) = channel ()
      val _ = RunCML.logChannel("CIO:reqCh", reqCh)
    in
    fun spawnStreamDB () = let
	(* find and remove the entry with the given id. *)
	  fun zapStream (id, l) = let
		fun find [] = []
		  | find ((x : strm_info_t)::r) = if (id = (#id x))
		      then r
		      else (x :: (find r))
		in
		  find l
		end
	  fun killStream {id, kill} = spawn kill
	  fun loop streams = (case (accept reqCh)
	       of (OPEN_STRM arg) => loop (arg::streams)
		| (REMOVE id) => loop (zapStream (id, streams))
		| (KILL_ALL ack) => (
		    app killStream streams;
		    syncLoop streams;
		    writeVar(ack, ())))
	  and syncLoop [] = ()
	    | syncLoop streams = (case (accept reqCh)
	       of (REMOVE id) => syncLoop (zapStream (id, streams))
		| _ => syncLoop streams)
	  in
	    spawn (fn () => loop []);
	    ()
	  end
    fun addStrm (id, killFn) = send (reqCh, OPEN_STRM{id=id, kill=killFn})
    fun removeStrm id = send (reqCh, REMOVE id)
    fun killAll () = let
	  val ack = condVar()
	  in
	    send (reqCh, KILL_ALL ack); readVar ack
	  end
    end (* local *)


  (** input streams **)

    val inBufSz = 2048

    datatype in_req_t
      = INPUT of {nbytes : int, abort : unit event, reply_ch : in_reply_t chan}
      | INPUT_LN of {abort : unit event, reply_ch : in_reply_t chan}
      | LOOKAHEAD of {abort : unit event, reply_ch : in_reply_t chan}
      | CAN_INPUT of int cond_var
      | CLOSE_IN
      | KILL_IN

    and in_reply_t = INPUT_REPLY of string | INPUT_ERR of string

    datatype instream = INSTRM of {
	name : string,
	req_ch : in_req_t chan
      }

    type inbuf_state_t = {    (* this is the state of an input buffer *)
	buf : string list,	(* the buffered bytes *)
	remain : int,		(* the number of remaining bytes *)
	pos : int		(* the next byte in the head of buf to *)
				(* read *)
      }

  (* extract n bytes from a buffer, returning the extracted string and the
   * new buffer state
   *)
    fun extract ({buf=[], ...}, _) = ("", {buf=[], remain=0, pos=0})
      | extract ({buf as (s::r), remain, pos}, n) = let
	  val hdRemain = (size s) - pos
	  in
	    if (hdRemain >= n)
	      then let
	        val newBuf = if (hdRemain = n)
		      then {buf = r, remain = remain-n, pos = 0}
		      else {buf = buf, remain = remain-n, pos = pos+n}
	        in
		  (substring(s, pos, n), newBuf)
	        end
	      else let
	        fun ext (_, [], sl) = (implode (rev sl), {buf=[], remain=0, pos=0})
		  | ext (k, s::r, sl) = let
		      val len = size s
		      in
		        if (len = k)
			  then
			    (implode (rev (s::sl)), {
			        buf = r, remain = remain-n, pos = 0
			      })
		        else if (len > k)
			  then
			    (implode (rev (substring(s, 0, k) :: sl)), {
			        buf = s::r, remain = remain-n, pos = k
			      })
			  else ext(k-len, r, s::sl)
		      end
		in
		  ext (n - hdRemain, r, [substring(s, pos, hdRemain)])
		end
	  end (* end extract *)

    fun insert ({buf, remain, pos}, s) =
	  {buf = (buf @ [s]), remain = remain + (size s), pos = pos}

    fun mkInBuf ("", _) = {buf = [], remain = 0, pos = 0}
      | mkInBuf (s, pos) = {buf = [s], remain = (size s) - pos, pos = pos}

    datatype input_t = DATA of string | EOF | ERR of string | ABORTED

  (* Spawn a input stream server thread.  The arguments are:
   *   get : (int * unit event) -> input_t  -- get the requested amount of input
   *   avail : unit -> int                  -- how many bytes are available?
   *   close : unit -> unit                 -- close the stream
   *   kill : unit -> unit                  -- kill the stream (no close for std_in)
   *   reqCh : in_req_t chan                -- client request channel
   *)
    fun spawnInServer {get, avail, close, kill, reqCh} = let
	  fun server () = let
		fun replyEvt (replyCh, s) = transmit(replyCh, INPUT_REPLY s)
		fun replyErrEvt (replyCh, msg) = transmit(replyCh, INPUT_ERR msg)
		fun nullReplyEvt replyCh = transmit(replyCh, INPUT_REPLY "")
		fun atEOF () = let
		      fun eofLoop () = (
			    case (accept reqCh)
			     of (INPUT{abort, reply_ch, ...}) =>
				  select[nullReplyEvt reply_ch, abort]
			      | (INPUT_LN{abort, reply_ch, ...}) =>
				  select[nullReplyEvt reply_ch, abort]
			      | (CAN_INPUT replyVar) => writeVar(replyVar, 0)
			      | (LOOKAHEAD{abort, reply_ch, ...}) =>
				  select[nullReplyEvt reply_ch, abort]
			      | CLOSE_IN => ()
			      | KILL_IN => exit()
			    (* end case *);
			    eofLoop ())
		      in
			close();
			eofLoop ()
		      end (* atEOF *)

		fun loop buf = (case (accept reqCh)
		       of (INPUT req) => loop (input (req, buf))
			| (INPUT_LN req) => loop (inputLn (req, buf))
			| (CAN_INPUT replyVar) => (
			    writeVar (replyVar, (#remain buf) + avail());
			    loop buf)
			| (LOOKAHEAD req) => loop (look (req, buf))
			| CLOSE_IN => atEOF()
			| KILL_IN => (kill(); exit())
		      (* end case *))

		and input (req as {nbytes, abort, reply_ch}, buf as {remain, ...}) =
		      if (remain >= nbytes)
			then let
			  val (s, newBuf) = extract(buf, nbytes)
			  in
			    select [
				wrap (abort, fn () => buf),
				wrap (replyEvt(reply_ch, s), fn () => newBuf)
			      ]
			  end
			else (case (get (max (inBufSz, nbytes-remain), abort))
			   of (DATA s) => input (req, insert (buf, s))
			    | EOF => select [
				    wrap (abort, fn _ => buf),
				    wrap (
				      replyEvt (reply_ch,
					#1 (extract (buf, remain))),
				      atEOF)
				  ]
			    | (ERR msg) => (
				select [abort, replyErrEvt (reply_ch, msg)];
				buf)
			    | ABORTED => buf
			  (* end case *))

		and inputLn ({abort, reply_ch}, buffer as {buf, pos, remain}) = let
		      fun scanForNL (s, i) = if (ordof(s, i) = 10)
			    then i+1
			    else scanForNL (s, i+1)
		      fun findNL ([], _, accum) = (case (get (inBufSz, abort))
			     of (DATA s) => findNL ([s], 0, accum)
			      | EOF => select [
				    wrap (abort, fn _ => mkInBuf (accum, 0)),
				    wrap (replyEvt (reply_ch, accum), atEOF)
				  ]
			      | (ERR msg) => (
				  select [abort, replyErrEvt (reply_ch, msg)];
				  mkInBuf (accum, 0))
			      | ABORTED => mkInBuf (accum, 0)
			    (* end case *))
			| findNL (s::r, pos, accum) = let
			    val i = scanForNL (s, pos)
			    val s0 = accum ^ substring (s, pos, i - pos)
			    val newBuf = if ((size s) = i)
				  then {
				      buf = r,
				      remain = case r of [] => 0 | (x::_) => size x,
				      pos = 0
				    }
				  else {
				      buf = s::r, remain = (size s) - i,
				      pos = i
				    }
			    in
			      select [
				  wrap (abort, fn _ => buffer),
				  wrap (replyEvt (reply_ch, s0), fn _ => newBuf)
				]
			    end
			      handle Ord => findNL (r, 0,
				  accum ^ substring(s, pos, (size s) - pos))
		      in
			findNL (buf, pos, "")
		      end (* inputLn *)

		and look (req as {abort, reply_ch}, buffer as {buf = [], ...}) = (
		      case (get (inBufSz, abort))
		       of (DATA s) => look (req, mkInBuf (s, 0))
			| EOF => (select [abort, nullReplyEvt reply_ch]; atEOF())
			| (ERR msg) => (
			    select [abort, replyErrEvt (reply_ch, msg)];
			    buffer)
			| ABORTED => buffer
		      (* end case *))
		  | look ({abort, reply_ch}, buffer as {buf = s::_, pos, ...}) = (
		      select [abort, replyEvt (reply_ch, substring(s, pos, 1))];
		      buffer)
		in
		  loop (mkInBuf ("", 0))
		end
	  in
	    spawn server
	  end (* spawnInServer *)

    fun getBytes fd (nbytes, abortEvt) = let
	  fun get () = (let
		val buf = System.Unsafe.Assembly.A.create_s nbytes
		in
		  case (SysIO.read (fd, stringToBytearray buf, nbytes))
		   of 0 => EOF
		    | n => DATA(if (n < nbytes) then substring(buf, 0, n) else buf)
		end
		  handle (SystemCall s) => ERR s)
	  in
	    select [
		wrap (abortEvt, fn () => ABORTED),
		wrap (syncOnInput fd, get)
	      ]
	  end

    fun open_in name = let
	  val fd = SysIO.openf (name, SysIO.O_READ)
		  handle (SystemCall s) =>
		    error("open_in", name, s)
	  val id = ref fd
	  val reqCh = channel()
	  fun closeFn () = (closef fd; removeStrm id)
	  in
	    addStrm (id, fn () => send (reqCh, KILL_IN));
	    spawnInServer {
		get = getBytes fd,
		avail = fn () => SysIO.fionread fd,
		close = closeFn,
		kill = closeFn,
		reqCh = reqCh
	      };
	    INSTRM{name = name, req_ch = reqCh}
	  end
    fun open_string data = let
	  val reqCh = channel()
	  fun closeFn () = ()
	  val pos = ref 0
	  val totLen = size data
	  fun avail () = (totLen - !pos)
	  fun getBytes (nbytes, _) = let
		val p = !pos
		val avail = (totLen - p)
		fun get 0 = EOF
		  | get n = (DATA(substring(data, p, n)) before (pos := p + n))
		in
		  if (avail < nbytes) then get avail else get nbytes
		end
	  in
	    spawnInServer {
		get = getBytes,
		avail = avail,
		close = closeFn,
		kill = closeFn,
		reqCh = reqCh
	      };
	    INSTRM{name = "<string>", req_ch = reqCh}
	  end

    exception ClosedStream

    fun openChanIn () = let
	  val ch = channel() and reqCh = channel()
	  val availCh = channel() and getCh = channel()
	  val isClosed = condVar()
	  fun buffer (_, [], []) = select [
		  wrap (receive ch, fn x => buffer (size x, [x], [])),
		  wrap (transmit (availCh, 0), fn () => buffer (0, [], []))
		]
	    | buffer (n, [], rear) = buffer (n, rev rear, [])
	    | buffer (n, front as (s::r), rear) = select [
		  wrap (receive ch, fn x => buffer (n + size x, front, x::rear)),
		  wrap (transmit (getCh, s), fn x => buffer (n - size s, r, rear)),
		  wrap (transmit (availCh, n), fn () => buffer (n, front, rear))
		]
	  fun sendFn s = select [
		  transmit (ch, s),
		  wrap (readVarEvt isClosed, fn () => raise ClosedStream)
		]
	  in
	    spawn (fn () => buffer (0, [], []));
	    spawnInServer {
		get = fn _ => DATA (accept getCh),
		avail = fn () => accept availCh,
		close = fn () => writeVar (isClosed, ()),
		kill = fn () => writeVar (isClosed, ()),
		reqCh = reqCh
	      };
	    (sendFn, INSTRM{name="<channel>", req_ch = reqCh})
	  end

    fun close_in (INSTRM{req_ch, ...}) = send(req_ch, CLOSE_IN)

  (* the standard input *)
    local
      val stdinFD = 0
      val stdinId = ref stdinFD
      val stdinReqCh : in_req_t chan = channel()
      val _ = RunCML.logChannel ("CIO.stdin-req", stdinReqCh)
    in
    val std_in = INSTRM{name = "<std_in>", req_ch = stdinReqCh}
    fun initInStreams () = (
	  addStrm (stdinId, fn () => send(stdinReqCh, KILL_IN));
	  spawnInServer {
	      get = getBytes stdinFD,
	      close = fn () => (closef stdinFD; removeStrm stdinId),
	      kill = fn () => removeStrm stdinId,
	      avail = fn () => SysIO.fionread stdinFD,
	      reqCh = stdinReqCh
	    })
    end

    fun getReply (replyCh, cmd, name) = (case (accept replyCh)
	   of (INPUT_REPLY s) => s
	    | (INPUT_ERR msg) => error (cmd, name, msg))

    fun can_input (INSTRM{req_ch, ...}) = let
	  val cv = condVar()
	  in
	    send (req_ch, CAN_INPUT cv);
	    readVar cv
	  end
    fun lookahead (INSTRM{req_ch, name}) = let
	  val replyCh = channel()
	  in
	    send (req_ch, LOOKAHEAD{abort = choose [], reply_ch = replyCh});
	    getReply (replyCh, "lookahead", name)
	  end
    fun checkInCnt (n, cmd, name) = if (n < 0)
	  then error (cmd, name, "negative character count")
	  else ()
    fun input (INSTRM{req_ch, name}, n) = let
	  val replyCh = channel ()
	  in
	    checkInCnt (n, "input", name);
	    send (req_ch, INPUT{nbytes = n, abort = choose[], reply_ch = replyCh});
	    getReply (replyCh, "input", name)
	  end
    fun inputc instrm n = input(instrm, n)
    fun input_line (INSTRM{req_ch, name}) = let
	  val replyCh = channel()
	  in
	    send (req_ch, INPUT_LN{abort = choose[], reply_ch = replyCh});
	    getReply (replyCh, "input_line", name)
	  end

    local
      fun reqInputEvt (INSTRM{req_ch, name}, reqFn, cmd) = guard (fn () => let
	    val abortCond = condVar()
	    fun abortFn () = writeVar(abortCond, ())
	    val replyCh = channel()
	    in
	      spawn(fn () => send (req_ch, reqFn(readVarEvt abortCond, replyCh)));
	      wrapAbort (
		wrap (receive replyCh,
		  fn (INPUT_REPLY s) => s
		   | (INPUT_ERR msg) => error (cmd, name, msg)),
		abortFn)
	    end)
    in
    fun lookaheadEvt instrm = reqInputEvt (
	  instrm, fn (a, r) => LOOKAHEAD{abort = a, reply_ch = r}, "lookaheadEvt")
    fun inputEvt (instrm as INSTRM{name, ...}, n) = (
	  checkInCnt (n, "input", name);
	  reqInputEvt (instrm,
	    fn (a, r) => INPUT{nbytes = n, abort = a, reply_ch = r}, "inputEvt"))
    fun inputcEvt instrm n = inputEvt (instrm, n)
    fun inputLineEvt instrm = reqInputEvt (
	  instrm, fn (a, r) => INPUT_LN{abort = a, reply_ch = r}, "inputLineEvt")
    end (* local *)

    fun end_of_stream (INSTRM{req_ch, name}) = let
	  val replyCh = channel()
	  in
	    send (req_ch, LOOKAHEAD{abort = choose [], reply_ch = replyCh});
	    case (accept replyCh)
	     of (INPUT_REPLY "") => true
	      | (INPUT_ERR msg) => error("end_of_stream", name, msg)
	      | _ => false
	  end


  (** output streams **)

    datatype out_req_t
      = FLUSH of string option cond_var
      | OUTPUT of {data : string, ack : string option cond_var}
      | CLOSE_OUT of string option cond_var
      | KILL_OUT

    datatype outstream = OUTSTRM of {
	name : string,
	req_ch : out_req_t chan
      }

    val outBufSz = 2048  (* amount of data to buffer before flushing *)

  (* Spawn an output stream server thread.  The arguments are
   *   write : string -> unit  -- write a string to the ouput device
   *   close : unit -> unit    -- close the stream
   *   kill : unit -> unit     -- kill the stream (no close for std_out & std_err)
   *   reqCh : in_req_t chan   -- client request channel
   *)
    fun spawnOutServer {write, close, kill, reqCh} = let
	  fun ok cv = writeVar(cv, NONE)
	  fun flushBuf buf = write (implode (rev buf))
	  fun hasNL s = let
		fun find i = (ordof(s, i) = 10) orelse find(i+1)
		in
		  (find 0) handle _ => false
		end
	  fun server () = let
		fun closed () = (case (accept reqCh)
		       of (FLUSH ack) => ok ack
			| (OUTPUT{ack, ...}) =>
			    writeVar (ack, SOME "closed outstream")
			| (CLOSE_OUT ack) => ok ack
			| KILL_OUT => exit()
		      (* end case *))
		fun loop {buf, tot_len} = (case (accept reqCh)
		       of (FLUSH ack) => let
			    val ackMsg = (flushBuf buf; NONE)
				    handle SystemCall s => (SOME s)
			    in
			      writeVar (ack, ackMsg);
			      loop {buf = [], tot_len = 0}
			    end
			| (OUTPUT{data, ack}) => let
			    val len = size data
			    in
			      if (len+tot_len >= outBufSz) orelse (hasNL data)
				then let
				  val ackMsg = (flushBuf (data::buf); NONE)
					  handle SystemCall s => (SOME s)
				  in
				    writeVar (ack, ackMsg);
				    loop {buf = [], tot_len = 0}
				  end
				else (
				  ok ack;
				  loop {buf = data::buf, tot_len = tot_len+len})
			    end
			| (CLOSE_OUT ack) => let
			    val ackMsg = (flushBuf buf; close(); NONE)
				    handle SystemCall s => (SOME s)
			    in
			      writeVar (ack, ackMsg);
			      closed()
			    end
			| KILL_OUT => (flushBuf buf; kill(); exit())
		      (* end case *))
		in
		  loop {buf = [], tot_len = 0}
		end
	  in
	    spawn server; ()
	  end

    fun writeStr fd s = (case (size s)
	   of 0 => ()
	    | 1 => SysIO.write (fd, stringToBytearray(s^"\000"), 1)
	    | n => SysIO.write (fd, stringToBytearray s, n)
	  (* end case *))

  (* open an outstream in the given mode *)
    local
      fun open_o (mode, cmd) name = let
	    val fd = (SysIO.openf (name, mode))
		    handle (SystemCall msg) => error(cmd, name, msg)
	    val id = ref fd
	    fun closeFn () = (closef fd; removeStrm id)
	    val reqCh = channel()
	    in
	      addStrm (id, fn () => send(reqCh, KILL_OUT));
	      spawnOutServer {
		  write = writeStr fd,
		  close = closeFn,
		  kill = closeFn,
		  reqCh = reqCh
		};
	      OUTSTRM{name = name, req_ch = reqCh}
	    end
    in
    val open_out = open_o (SysIO.O_WRITE, "open_out")
    val open_append = open_o (SysIO.O_APPEND, "open_append")
    end

    fun openChanOut () = let
	  val ch = channel() and reqCh = channel()
	  in
	    spawnOutServer {
		write = (fn "" => () | s => send(ch, s)),
		close = fn () => (),
		kill = fn () => (),
		reqCh = reqCh
	      };
	    (receive ch, OUTSTRM{name = "<channel>", req_ch = reqCh})
	  end

  (* the standard output streams *)
    local
      val stdoutFD = 1 and stderrFD = 2
      val stdoutId = ref stdoutFD and stderrId = ref stderrFD
      val stdoutReqCh : out_req_t chan = channel()
      val stderrReqCh : out_req_t chan = channel()
      val _ = RunCML.logChannel ("CIO.stdout-req", stdoutReqCh)
      val _ = RunCML.logChannel ("CIO.stderr-req", stderrReqCh)
    in
    val std_out = OUTSTRM{name = "<std_out>", req_ch = stdoutReqCh}
    val std_err = OUTSTRM{name = "<std_err>", req_ch = stderrReqCh}
    fun initOutStreams () = (
	  spawnOutServer {
	      write = writeStr stdoutFD,
	      close = fn () => (closef stdoutFD; removeStrm stdoutId),
	      kill = fn () => removeStrm stdoutId,
	      reqCh = stdoutReqCh
	    };
	  spawnOutServer {
	      write = writeStr stderrFD,
	      close = fn () => (closef stderrFD; removeStrm stderrId),
	      kill = fn () => removeStrm stderrId,
	      reqCh = stderrReqCh
	    })
    end

    local
      fun sendReq (cmd, OUTSTRM{name, req_ch}, reqFn) = let
	    val cv = condVar()
	    in
	      send(req_ch, reqFn cv);
	      case (readVar cv)
	       of NONE => ()
		| (SOME ex) => error (cmd, name, ex)
	    end
    in
    fun close_out outstrm = sendReq ("close", outstrm, CLOSE_OUT)
    fun output (outstrm, s) =
	  sendReq ("output", outstrm, fn cv => OUTPUT{data=s, ack=cv})
    fun outputc outstrm s =
	  sendReq ("outputc", outstrm, fn cv => OUTPUT{data=s, ack=cv})
    fun flush_out outstrm = sendReq ("flush_out", outstrm, FLUSH)
    end (* local *)

    val print = outputc std_out


    fun execute_in_env (cmd, args, env) = let
	  open System.Unsafe.CInterface
	  fun basename s = let
	 	fun f [] = s
		  | f ("/"::r) = g (r, r)
		  | f (_::r) = f r
		and g ([], base) = implode base
		  | g ("/"::r, _) = g(r, r)
		  | g (_::rest, base) = g(rest, base)
		in
		  f (explode s)
		end
	  val cArgs = map c_string ((basename cmd) :: args)
	  val cEnv = map c_string env
	  val (inFD, outFD) = exec (c_string cmd, cArgs, cEnv)
		handle (SysError(_,msg)) => error ("execute", cmd, msg)
	  val (inId, outId) = (ref inFD, ref outFD)
	  val inReqCh = channel() and outReqCh = channel()
	  val instrm = INSTRM{name = "<pipe_in>", req_ch = inReqCh}
	  val outstrm = OUTSTRM{name = "<pipe_out>", req_ch = outReqCh}
	  fun closeInFn () = (closef inFD; removeStrm inId)
	  fun closeOutFn () = (closef outFD; removeStrm outId)
	  in
	    addStrm (inId, fn () => send (inReqCh, KILL_IN));
	    spawnInServer {
		get = getBytes inFD,
		avail = fn () => SysIO.fionread inFD,
		close = closeInFn,
		kill = closeInFn,
		reqCh = inReqCh
	      };
	    addStrm (outId, fn () => send(outReqCh, KILL_OUT));
	    spawnOutServer {
		write = writeStr outFD,
		close = closeOutFn,
		kill = closeOutFn,
		reqCh = outReqCh
	      };
	    (instrm, outstrm)
	  end (* execute *)

    fun execute (cmd, args) = execute_in_env (cmd, args, System.environ())


  (** initialization code **)

    fun startup () = (spawnStreamDB(); initInStreams(); initOutStreams())

    val _ = RunCML.logServer ("CIO", startup, killAll)

  end (* functor ConcurIO *)
