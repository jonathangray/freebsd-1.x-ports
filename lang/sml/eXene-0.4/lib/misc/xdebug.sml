(* xdebug.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure XDebug =
  struct

  (* the root of all eXene trace modules *)
    val eXeneTM = TraceCML.traceModule (TraceCML.traceRoot, "eXene")

  (* a trace module for controlling the printing of error messages *)
    val errorTM = TraceCML.traceModule (eXeneTM, "errors")

  (* a trace module for controlling xspawn output *)
    val xspawnTM = TraceCML.traceModule (eXeneTM, "xspawn")

  (* eXene library-level trace modules *)
    val libTM = TraceCML.traceModule (eXeneTM, "lib")
    val ioTM = TraceCML.traceModule (libTM, "io")
    val fontTM = TraceCML.traceModule (libTM, "font")
    val colorTM = TraceCML.traceModule (libTM, "color")
    val drawTM = TraceCML.traceModule (libTM, "draw")
    val dmTM = TraceCML.traceModule (libTM, "draw-master")
    val winregTM = TraceCML.traceModule (libTM, "winreg")
    val topTM = TraceCML.traceModule (libTM, "top-level")
    val gcTM = TraceCML.traceModule (libTM, "gc")

  (* the root of the widgets trace modules *)
    val widgetsTM = TraceCML.traceModule (eXeneTM, "widgets")

    val trace = TraceCML.trace
    fun errTrace f = trace(errorTM, f)

    fun reset () = (
	  TraceCML.traceOff eXeneTM;
	  TraceCML.traceOn errorTM)

  (* initialiize the state of the trace modules according to the argument
   * list.  The format of an argument is:
   *   [!|-|+]name
   * where
   *   "-name" means TraceCML.traceOff "name"
   *   "+name" means TraceCML.traceOn "name"
   *   "!name" means TraceCML.traceOnly "name"
   * and "name" is an abbreviation for "+name".
   *)
    fun init args = let
	  fun tail s = substring(s, 1, size s - 1)
	  fun doArg s = (case (ord s)
		 of (* "+" *)43 => TraceCML.traceOn(TraceCML.moduleOf(tail s))
		  | (* "-" *)45 => TraceCML.traceOff(TraceCML.moduleOf(tail s))
		  | (* "!" *)33 => TraceCML.traceOnly(TraceCML.moduleOf(tail s))
		  | _ => TraceCML.traceOn (TraceCML.moduleOf s)
		(* end case *))
	  in
	    reset();
	    app doArg args
	  end
(***
    val listLen = ref 16
    val lineLen = ref 20

    fun prBuf lvl s = let
	  val pr = pr lvl
	  fun f (i, 1, 0) = (pr "\n  "; pr(makestring(ordof(s, i))))
	    | f (i, 1, _) = pr(makestring(ordof(s, i)))
	    | f (i, n, 0) = (pr "\n  "; f (i, n, !lineLen))
	    | f (i, n, k) = (
		pr(makestring(ordof(s, i)));
		pr ", ";
		f(i+1, n-1, k-1))
	  val n = String.size s
	  in
	    pr "[ ";
	    if (n <= !listLen)
	      then (f(0, n, !lineLen); pr " ]\n")
	      else (f(0, !listLen, !lineLen); pr " ...]\n")
	  end
***)

    local
      fun handleXERROR (tid, MLXError.XERROR s) = (
	    TraceCML.trace(errorTM, fn () => [
		"exception (XERROR ", s, ") in ", CML.tidToString tid, "\n"
	      ]);
	    true)
	| handleXERROR _ = false
    in
    val _ = TraceCML.setHandleFn handleXERROR
    end;

    fun xspawn (name, f) = let
	  open CML
	  fun wrapf () = (
		trace (xspawnTM, fn () => [
		    "xspawn ", name, " ", tidToString(getTid()), "\n"
		  ]);
		f ())
		  handle (MLXError.XERROR s) => trace (errorTM, fn () => [
			    "exception (XERROR ", s, ") in ", name, " thread\n"
			  ])
		       | ex => trace (errorTM, fn () => [
			    "exception ", System.exn_name ex, " in ", name,
			    " thread\n"
			  ])
	  val tid = spawn wrapf
	  in
	    TraceCML.watch (name, tid);
	    tid
	  end

  (* wrapper to report uncaught exceptions *)
    fun diag (f, s) x = (f x) handle ex => (
	  trace (errorTM, fn () => [
	      "exception ", System.exn_name ex, " in ", s, "\n"
	    ]);
	  raise ex)

  end (* XDebug *)
