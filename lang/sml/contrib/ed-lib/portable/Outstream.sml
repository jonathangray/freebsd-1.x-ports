(*$Outstream: OUTSTREAM General *)

loadSig "OUTSTREAM";

structure Outstream: OUTSTREAM =

(* OUTPUT STREAMS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Nov 1989

Maintenance:	Author


SEE ALSO

   Instream, StreamPair


RCS LOG

$Log: Outstream.sml,v $
Revision 1.1  1994/02/08 00:23:20  jkh
Initial revision

Revision 1.6  91/02/12  18:20:03  18:20:03  db (Dave Berry)
Used NonStandard.flush_out to implement the flush function.

Revision 1.5  91/02/11  21:25:58  21:25:58  db (Dave Berry)
Now uses the pervasive implementations of streams.  I now believe that
any problems with end_of_stream on interactive streams, or error
messages, are the responsibility of the compiler writer.

Revision 1.4  91/02/04  17:01:13  17:01:13  db (Dave Berry)
Renamed Outstream to outstream, and similarly for structure and signature 
ids.
Renamed output' to write.
Added Io exception.

Revision 1.3  91/01/25  20:19:15  20:19:15  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:25:17  17:25:17  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  15:02:24  15:02:24  db (Dave Berry)
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

  fun openAppend s = raise NotImplemented "openAppend"
  fun eof os = raise NotImplemented "eof"
  fun interactive os = raise NotImplemented "interactive"

end
