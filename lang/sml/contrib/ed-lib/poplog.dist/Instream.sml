(*$Instream: INSTREAM General *)

loadSig "INSTREAM";

structure Instream: INSTREAM =

(* INPUT STREAMS FOR POPLOG ML

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        26 Mar 1991

Maintenance:	Author


DESCRIPTION

   Poplog ML provides implementations of interactive and input_line.


SEE ALSO

   Outstream, StreamPair


RCS LOG

$Log: Instream.sml,v $
Revision 1.1  1994/02/08 00:23:15  jkh
Initial revision

Revision 1.1  91/04/10  16:58:36  16:58:36  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type instream = instream


(* SYSTEM *)

  exception NotImplemented of string

  fun openString s = raise NotImplemented "openString"
  fun reset i = raise NotImplemented "reset"
  fun read i n = raise NotImplemented "read"
  fun canInput i n = raise NotImplemented "canInput"
  fun interactive i = StdIO.is_term_in i

(* MANIPULATORS *)

  exception Io = Io

  val stdIn = std_in
  val std_in = stdIn

  val openIn = open_in
  val open_in = openIn

  val closeIn = close_in
  val close_in = closeIn

  val input = input

  fun input1 i = input (i, 1)

  val lookahead = lookahead

  val end_of_stream = end_of_stream
  val eof = end_of_stream

  local
    fun readString' _ [] = OK ()
    |   readString' i (h::t) =
	  case input1 i
	  of "" => Fail ""
	  |  c  =>
	      if h = c then 
		case readString' i t of
		  OK () => OK ()
		| Fail s => Fail (c ^ s)
	      else
		Fail c
  in
    fun readString i s = readString' i (explode s)
  end

  fun skip p i =
	case lookahead i
	of "" => ()
	|  s  => if p s then (input1 i; skip p i) else ()

  fun inputLine i = StdIO.input_line i
end
