(*$Outstream: OUTSTREAM General *)

loadSig "OUTSTREAM";

structure Outstream: OUTSTREAM =

(* OUTPUT STREAMS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Nov 1989

Maintenance:	Author


DESCRIPTION

   SML/NJ supports the functionality of openAppend and interactive.


SEE ALSO

   Instream, StreamPair


RCS LOG

$Log: Outstream.sml,v $
Revision 1.1  1994/02/08 00:23:13  jkh
Initial revision

Revision 1.1  91/09/13  14:19:44  14:19:44  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type outstream = outstream


(* MANIPULATORS *)

  exception Io = Io

  val stdOut = std_out
  val std_out = stdOut

  val openOut = open_out
  val open_out = openOut

  val closeOut = close_out
  val close_out = closeOut

  val output = output

  val write = General.curry output


(* SYSTEM *)

  exception NotImplemented of string

  val flush = NonStandard.flush_out

  val openAppend = IO.open_append

  val interactive = IO.is_term_out

  fun eof os = raise NotImplemented "eof"

end
