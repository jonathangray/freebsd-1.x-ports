(*$OutstreamType : OUTSTREAM_TYPE Outstream *)

loadEntry "Outstream";

(* No need to do anything ifOutstream contains the real pervasives. *)


(* OUTSTREAM TYPE

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk

Date:           14 Nov 1989

Maintenance:    Author

RCS LOG

$Log: OutstreamType.sml,v $
Revision 1.1  1994/02/08 00:23:20  jkh
Initial revision

Revision 1.5  91/02/11  21:26:01  21:26:01  db (Dave Berry)
Now uses the pervasive implementations of streams.  I now believe that
any problems with end_of_stream on interactive streams, or error
messages, are the responsibility of the compiler writer.

Revision 1.4  91/02/04  16:57:43  16:57:43  db (Dave Berry)
This signature now defines all the pervasives on outstreams.  So an
implementation of this signature can replace the pervasives if necessary.

Revision 1.3  91/01/30  19:01:25  19:01:25  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.

Revision 1.2  91/01/25  20:19:22  20:19:22  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.1  90/12/20  15:02:37  15:02:37  db (Dave Berry)
Initial revision


*)
