(*$OUTSTREAM *)

signature OUTSTREAM =
sig

(* OUTPUT STREAMS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Nov 1989

Maintenance:	Author


DESCRIPTION

   Functions on output streams.


SEE ALSO

   INSTREAM, STREAM_PAIR


NOTES

   All counterparts of functions in the initial basis have been given both
   the original names and names that fit the library conventions.


RCS LOG

$Log: OUTSTREAM.sml,v $
Revision 1.1  1994/02/08 00:23:25  jkh
Initial revision

Revision 1.5  91/02/04  16:43:16  16:43:16  db (Dave Berry)
Renamed OutStream to outstream and similarly for structure and signature ids.
Renamed output' to write.

Revision 1.4  91/01/26  13:43:50  13:43:50  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.3  91/01/25  16:55:25  16:55:25  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:06:41  17:06:41  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:53:22  16:53:22  db (Dave Berry)
Initial revision


*)

(* PERVASIVES *)

  type outstream

  exception Io of string

  val std_out: outstream
  val output: outstream * string -> unit
  val open_out: string -> outstream
  val close_out: outstream -> unit


(* MANIPULATORS *)

  val stdOut: outstream
   (* stdOut = std_out *)

  val openOut: string -> outstream
   (* openOut = open_out *)

  val closeOut: outstream -> unit
   (* closeOut = close_out *)

  val write: outstream -> string -> unit
   (* write os s = curry output *)


(* SYSTEM *)

  exception NotImplemented of string
   (* NotImplemented fn; raised if fn is not provided. *)

  val openAppend: string -> outstream
   (* openAppend file; returns a new outstream whose consumer is the file s.
      Output to this stream is appended to s.  If s doesn't exist, it is
      created, initially empty. *)

  val flush: outstream -> unit
   (* flush os; ensures that all characters output on os have been or can
      be received by the associated consumer without being delayed by the
      implementation.  For example, characters output to a stream connected
      to a terminal will appear on that screen, even if they are normally
      buffered. *)

  val eof: outstream -> unit
   (* eof os: signal an end of stream on os without closing os. *)

  val interactive: outstream -> bool
   (* interactive os; returns true if os is associated with a terminal. *)
end

