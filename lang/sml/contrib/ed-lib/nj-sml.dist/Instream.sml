(*$Instream: INSTREAM General *)

loadSig "INSTREAM";

structure Instream: INSTREAM =

(* INPUT STREAMS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Nov 1989

Maintenance:	Author


DESCRIPTION

   SML/NJ provides the functionality of openString, canInput,
   interactive and read.


SEE ALSO

   Outstream, StreamPair


RCS LOG

$Log: Instream.sml,v $
Revision 1.1  1994/02/08 00:23:14  jkh
Initial revision

Revision 1.1  91/09/13  14:18:49  14:18:49  db (Dave Berry)
Initial revision



*)

struct


(* TYPES *)

  type instream = instream


(* SYSTEM *)

  exception NotImplemented of string

  val openString = IO.open_string

  fun canInput i n = IO.can_input i > n

  fun reset i = raise NotImplemented "reset"

  val interactive = IO.is_term_in

  val read = IO.inputc


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

  val inputLine = IO.input_line

end
