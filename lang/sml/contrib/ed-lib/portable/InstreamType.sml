(*$InstreamType : INSTREAM_TYPE Instream *)

loadEntry "Instream";

(* No need to do anything if Instream contains the real pervasives. *)

(* INSTREAM TYPE

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Sep 1989

Maintenance:    Author

RCS LOG

$Log: InstreamType.sml,v $
Revision 1.1  1994/02/08 00:23:18  jkh
Initial revision

Revision 1.5  91/02/11  21:25:56  21:25:56  db (Dave Berry)
Now uses the pervasive implementations of streams.  I now believe that
any problems with end_of_stream on interactive streams, or error
messages, are the responsibility of the compiler writer.

Revision 1.4  91/02/04  16:56:05  16:56:05  db (Dave Berry)
This signature now defines all the pervasives on instreams.  So an
implementation of this signature can replace the pervasives if necessary.

Revision 1.3  91/01/30  19:01:20  19:01:20  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.

Revision 1.2  91/01/25  20:17:18  20:17:18  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.1  90/12/20  14:53:46  14:53:46  db (Dave Berry)
Initial revision


*)

