(* xserver-addr.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure XServerAddr =
  struct

    datatype server_addr
      = UNIX of string
      | INET of (string * string)

    exception BadAddr of string

    local
      open Format

      val xTCPPort = 6000
      val xUNIXPath = "/tmp/.X11-unix/X"

      fun findChar c (s, j)= let
	    val cc = ord c
	    fun find i = if (ordof(s, i) = cc) then i else find(i+1)
	    in
	      (find j)
	    end

      val atoi = #1 o (StringCvt.strToInt StringCvt.Dec)

      fun mkUNIXAddr (dpyNum : int, scr : int) = {
	      addr = UNIX(xUNIXPath ^ (makestring dpyNum)),
	      dpy_name = format "unix:%d.%d" [INT dpyNum, INT scr],
	      screen = scr
	    }
      fun mkINETAddr (host, dpyNum, scr : int) = {
	      addr = INET(host, makestring(xTCPPort+dpyNum)),
	      dpy_name = format "%s:%d.%d" [STR host, INT dpyNum, INT scr],
	      screen = scr
	    }
    in

    fun getServerAddr "" = mkUNIXAddr(0, 0)
      | getServerAddr s = let
	  val i = findChar ":" (s, 0)
	  val hostname = (case i of 0 => NONE | j => SOME(substring(s, 0, i)))
	  val i = i+1
	  val j = (findChar "." (s, i)) handle Ord => (size s)
	  val dpy = if (i = j)
		then (raise BadAddr "missing display")
		else atoi(substring(s, i, j-i), 0)
	  val scr = if (j = (size s)) then 0 else atoi(s, j+1)
	  in
	    case hostname
	     of NONE => mkUNIXAddr(dpy, scr)
	      | (SOME "unix") => mkUNIXAddr(dpy, scr)
	      | (SOME name) => mkINETAddr(name, dpy, scr)
	  end
	    handle Ord => raise BadAddr "bad address string"

    end (* local *)
  end (* XServerAddr *)
