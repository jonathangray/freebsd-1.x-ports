(*$Outstream: OUTSTREAM General *)

loadSig "OUTSTREAM";

structure Outstream: OUTSTREAM =

(* OUTPUT STREAMS FOR POPLOG ML

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Nov 1989

Maintenance:	Author


DESCRIPTION

   Poplog ML provides the interactive and open_append functions.


SEE ALSO

   Instream, StreamPair


RCS LOG

$Log: Outstream.sml,v $
Revision 1.1  1994/02/08 00:23:15  jkh
Initial revision

Revision 1.1  91/04/10  16:59:05  16:59:05  db (Dave Berry)
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

  val openAppend = StdIO.open_append
  val interactive = StdIO.is_term_out

  fun eof os = raise NotImplemented "eof"
end
