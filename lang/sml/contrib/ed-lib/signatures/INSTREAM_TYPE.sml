(*$INSTREAM_TYPE: Instream *)

signature INSTREAM_TYPE =

(* INSTREAM TYPE

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        21 Feb 1989

Maintenance:	Author


DESCRIPTION

   The instream type and the functions defined in the initial basis.


RCS LOG

$Log: INSTREAM_TYPE.sml,v $
Revision 1.1  1994/02/08 00:23:25  jkh
Initial revision

Revision 1.4  91/02/11  18:35:29  18:35:29  db (Dave Berry)
Renamed InStream to instream, etc.
Added specifications of pervasive functions, so that this entry can replace
the pervasives entirely.

Revision 1.3  91/01/25  19:16:30  19:16:30  db (Dave Berry)
Added dependence on OutStream.

Revision 1.2  91/01/25  16:55:11  16:55:11  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.1  90/12/17  16:48:24  16:48:24  db (Dave Berry)
Initial revision

*)

sig

  (* PERVASIVES *)

  type instream
    sharing type instream = Instream.instream

  val std_in: instream

  val open_in: string -> instream

  val input: instream * int -> string

  val lookahead: instream -> string

  val close_in: instream -> unit

  val end_of_stream: instream -> bool
end
