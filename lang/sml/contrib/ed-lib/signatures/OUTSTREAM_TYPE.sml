(*$OUTSTREAM_TYPE: Outstream *)

signature OUTSTREAM_TYPE =

(* OUTSTREAM TYPE

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        21 Feb 1989

Maintenance:	Author


DESCRIPTION

   The outstream type defined in Outstream and made available globally.

RCS LOG

$Log: OUTSTREAM_TYPE.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

Revision 1.4  91/02/11  18:37:50  18:37:50  db (Dave Berry)
Renamed OutStream to outstream, etc.
Added specifications of pervasive functions, so that this entry can replace
the pervasives entirely.

Revision 1.3  91/01/25  19:16:15  19:16:15  db (Dave Berry)
Added dependence on OutStream.

Revision 1.2  91/01/25  16:55:28  16:55:28  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.1  90/12/17  16:53:30  16:53:30  db (Dave Berry)
Initial revision


*)

sig

  (* PERVASIVES *)

  type outstream
    sharing type outstream = Outstream.outstream

  val std_out: outstream

  val open_out: string -> outstream

  val output: outstream * string -> unit

  val close_out: outstream -> unit
end
