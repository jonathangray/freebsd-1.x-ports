(* get-display.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * This structure provides ML functions for extracting the display
 * from the DISPLAY environment variable in the format required by
 * eXene.  Which version you use depends on whether your site runs NIS
 * (aka Yellow Pages).
 *
 * This file depends on the following modules from the SML/NJ Library:
 *    CType
 *    Pathname
 *    StringUtil
 *    UnixPath
 *    UnixEnv
 *)

structure GetDisplay : sig

    val idFromNIS : string -> string
	(* return the host string corresponding to the given 
         * display name using NIS to lookup the host's IP address *)

    val idFromHosts : string -> string
	(* return the host string corresponding to the given
         * display name by looking in /etc/hosts for the host's
	 * IP address
	 *)

    val fromNIS : unit -> string
	(* Calls idFromNIS using value of DISPLAY environment variable *)

    val fromHosts : unit -> string
	(* Calls idFromHosts using value of DISPLAY environment variable *)

  end = struct

    structure CI = System.Unsafe.CInterface

    val searchPath = Pathname.mkSearchPath "/bin:/usr/bin"

    fun getCmd cmd = (
	  (UnixPath.findFile (searchPath, [UnixPath.A_READ, UnixPath.A_EXEC]) cmd)
	    handle _ => raise Fail(cmd^" not found"))

    fun getHost (cmd, host, arg2) = let
	  val (inS, outS) = IO.execute (getCmd cmd, [host, arg2])
	  fun close () = (IO.close_in inS; IO.close_out outS)
	  in
	    (case (IO.input_line inS)
	     of "" => raise Fail "No host IP address"
	      | s => (
		  close();
		  substring(s, 0, StringUtil.indexp CType.isSpaceOrd (s, 0)))
	    (* end case *))
	      handle ex => (close(); raise ex)
	  end

    fun ypmatch host = getHost ("ypmatch", host, "hosts")
    fun grep host = getHost ("grep", host, "/etc/hosts")

    fun getDisplayVar () =
	  (case (UnixEnv.getEnv "DISPLAY")
	   of SOME s => s
	    | _ => raise Fail "No DISPLAY variable"
	  (* end case *))

    fun getDisplay getHost dpy =
	    case (StringUtil.tokenize ":" (dpy, 0))
	     of [_] => dpy
	      | ["unix", scr] => dpy
	      | [host, scr] => implode[getHost host, ":", scr]
	      | _ => raise Fail "Bad DISPLAY value"
	    (* end case *)
	  (* getDisplay *)


    val idFromNIS = getDisplay ypmatch

    val idFromHosts = getDisplay grep

    val fromNIS = idFromNIS o getDisplayVar

    val fromHosts = idFromHosts o getDisplayVar

  end; (* GetDisplay *)
